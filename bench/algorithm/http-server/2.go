package main

import (
	"encoding/json"
	"fmt"
	"math/rand"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/valyala/fasthttp"
)

var (
	client = &fasthttp.Client{
		ReadTimeout:                   time.Second,
		WriteTimeout:                  time.Second,
		MaxIdleConnDuration:           time.Minute,
		NoDefaultUserAgentHeader:      true,
		DisableHeaderNamesNormalizing: true,
		DisablePathNormalizing:        true,
		Dial: (&fasthttp.TCPDialer{
			Concurrency:      4096,
			DNSCacheDuration: time.Hour,
		}).Dial,
	}
)

func api(w http.ResponseWriter, r *http.Request) {
	n := -1
	decoder := json.NewDecoder(r.Body)
	var payload Payload
	if decoder.Decode(&payload) == nil {
		n = payload.Value
	}
	w.Write([]byte(fmt.Sprintf("%d", n)))
}

func runServer(port int) {
	http.HandleFunc("/api", api)
	if err := http.ListenAndServe(fmt.Sprintf("localhost:%d", port), nil); err != nil {
		panic(err)
	}
}

func send(api string, value int, ch chan<- int) {
	ret := -1
	payload := Payload{Value: value}
	payloadBytes, _ := json.Marshal(payload)

	req := fasthttp.AcquireRequest()
	defer fasthttp.ReleaseRequest(req)
	req.SetRequestURI(api)
	req.Header.SetMethod(fasthttp.MethodPost)
	req.SetBodyRaw(payloadBytes)

	resp := fasthttp.AcquireResponse()
	defer fasthttp.ReleaseResponse(resp)

	for {
		err := client.DoTimeout(req, resp, time.Second)
		if err == nil && resp.StatusCode() == 200 {
			if ret, err = strconv.Atoi(string(resp.Body())); err == nil {
				ch <- ret
				return
			}
		}
	}
}

func main() {
	n := 10
	if len(os.Args) > 1 {
		n, _ = strconv.Atoi(os.Args[1])
	}
	_ = n
	rand.Seed(time.Now().UTC().UnixNano())
	port := 30000 + rand.Intn(10000)
	// println(port)
	go runServer(port)
	api := fmt.Sprintf("http://localhost:%d/api", port)
	url := fasthttp.AcquireURI()
	url.Parse(nil, []byte(api))
	ch := make(chan int, n)
	for i := 1; i <= n; i++ {
		go send(api, i, ch)
	}
	sum := 0
	for i := 1; i <= n; i++ {
		sum += <-ch
	}
	fmt.Println(sum)
}

type Payload struct {
	Value int `json:"value"`
}
