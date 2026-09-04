package heartbeatmiddleware

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"time"
)

type Config struct {
	ReportEndpoint string `json:"reportEndpoint"`
}

func CreateConfig() *Config {
	return &Config{
		ReportEndpoint: "",
	}
}

type HeartbeatMiddleware struct {
	next   http.Handler
	config *Config
}

var started = false
var hosts = NewHostSet()

func (hm *HeartbeatMiddleware) reportHosts() error {
	hosts := hosts.GetHosts()
	json, err := json.Marshal(hosts)
	if err != nil {
		return err
	}
	res, err := http.Post(hm.config.ReportEndpoint, "application/json", bytes.NewBuffer(json))
	if err != nil {
		return err
	}
	if res.StatusCode != 200 {
		return fmt.Errorf("Got non-200 status code: %d\n", res.StatusCode)
	}
	return nil
}

func New(ctx context.Context, next http.Handler, config *Config, name string) (http.Handler, error) {
	hm := &HeartbeatMiddleware{
		next:   next,
		config: config,
	}

	if config.ReportEndpoint != "" && !started {
		started = true
		go func() {
			for {
				err := hm.reportHosts()
				if err != nil {
					fmt.Printf("Failed to report hosts: %e", err)
				}
				time.Sleep(time.Second * 30)
			}
		}()
	}
	return hm, nil
}

func (hm *HeartbeatMiddleware) ServeHTTP(rw http.ResponseWriter, req *http.Request) {
	hosts.AddHost(req.Host)
	hm.next.ServeHTTP(rw, req)
}
