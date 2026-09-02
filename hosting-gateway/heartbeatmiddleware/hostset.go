package heartbeatmiddleware

import "sync"

type HostSet struct {
	lock  *sync.Mutex
	hosts map[string]struct{}
}

func NewHostSet() *HostSet {
	return &HostSet{
		lock:  &sync.Mutex{},
		hosts: map[string]struct{}{},
	}
}

func (h *HostSet) GetHosts() []string {
	h.lock.Lock()
	defer h.lock.Unlock()
	results := make([]string, 0, len(h.hosts))
	for host := range h.hosts {
		results = append(results, host)
	}
	h.hosts = map[string]struct{}{}
	return results
}

func (h *HostSet) AddHost(host string) {
	h.lock.Lock()
	defer h.lock.Unlock()
	h.hosts[host] = struct{}{}
}
