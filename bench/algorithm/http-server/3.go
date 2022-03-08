package main

import (
	"encoding/json"
	"fmt"
	"math/rand"
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

func requestHandler(ctx *fasthttp.RequestCtx) {
	req := ctx.Request
	var payload Payload
	if json.Unmarshal(req.Body(), &payload) == nil {
		fmt.Fprintf(ctx, "%d", payload.Value)
	}
}

func runServer(port int) {
	if err := fasthttp.ListenAndServe(fmt.Sprintf("localhost:%d", port), requestHandler); err != nil {
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
	port := 20000 + rand.Intn(30000)
	go runServer(port)
	api := fmt.Sprintf("http://localhost:%d/", port)
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
