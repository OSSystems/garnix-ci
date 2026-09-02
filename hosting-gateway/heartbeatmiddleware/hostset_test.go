package heartbeatmiddleware

import (
	"sort"
	"sync"
	"testing"
)

func TestGetHostsDrainsTheSet(t *testing.T) {
	hosts := NewHostSet()
	hosts.AddHost("a.example")
	hosts.AddHost("b.example")

	first := hosts.GetHosts()
	sort.Strings(first)
	if len(first) != 2 || first[0] != "a.example" || first[1] != "b.example" {
		t.Fatalf("expected both hosts, got %v", first)
	}

	// Draining is the point: a host reported once must not keep a server alive
	// forever. The next report only carries hosts seen since.
	if second := hosts.GetHosts(); len(second) != 0 {
		t.Fatalf("expected the set to be drained, got %v", second)
	}
}

func TestAddHostDeduplicates(t *testing.T) {
	hosts := NewHostSet()
	for i := 0; i < 5; i++ {
		hosts.AddHost("a.example")
	}
	if got := hosts.GetHosts(); len(got) != 1 {
		t.Fatalf("expected one host, got %v", got)
	}
}

func TestConcurrentAddAndDrain(t *testing.T) {
	// Every request handled by the middleware writes to this set while the
	// reporting goroutine drains it, so racing the two must not corrupt it.
	hosts := NewHostSet()
	var wg sync.WaitGroup
	for i := 0; i < 50; i++ {
		wg.Add(1)
		go func(i int) {
			defer wg.Done()
			hosts.AddHost("host")
			if i%10 == 0 {
				hosts.GetHosts()
			}
		}(i)
	}
	wg.Wait()
	hosts.GetHosts()
}
