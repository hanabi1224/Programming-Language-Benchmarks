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
)

var (
	client = http.Client{
		Timeout: 10 * time.Second,
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
	api := fmt.Sprintf("http://localhost:%d/api", port)
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
