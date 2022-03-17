package main

import (
	"bytes"
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
	client = http.Client{
		Timeout: 10 * time.Second,
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
	for {
		if resp, err := client.Post(api, "", bytes.NewReader(payloadBytes)); err == nil {
			if resp.StatusCode == 200 {
				decoder := json.NewDecoder(resp.Body)
				decoder.Decode(&ret)
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
