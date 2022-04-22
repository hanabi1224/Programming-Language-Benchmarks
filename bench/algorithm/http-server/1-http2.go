package main

import (
	"bytes"
	"crypto/tls"
	"encoding/json"
	"fmt"
	"math/rand"
	"net"
	"net/http"
	"os"
	"strconv"
	"time"

	"golang.org/x/net/http2"
)

var (
	client = http.Client{
		Transport: createH2Transport(),
		Timeout:   10 * time.Second,
	}
)

func createH2Transport() *http2.Transport {
	// http2.Transport does not support http/1
	transport := &http2.Transport{
		TLSClientConfig: &tls.Config{InsecureSkipVerify: true},
	}
	return transport
}

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
	server := &http.Server{}
	http2.ConfigureServer(server, nil)
	listener, err := net.Listen("tcp", fmt.Sprintf("localhost:%d", port))
	if err != nil {
		panic(err)
	}
	if err := server.ServeTLS(listener, "cert.pem", "key.pem"); err != nil {
		panic(err)
	}
}

func send(api string, value int, ch chan<- int) {
	ret := -1
	payload := Payload{Value: value}
	payloadBytes, _ := json.Marshal(payload)
	for {
		resp, err := client.Post(api, "", bytes.NewReader(payloadBytes))
		// if err != nil {
		// 	println(err)
		// }
		if err == nil && resp.StatusCode == 200 {
			decoder := json.NewDecoder(resp.Body)
			decoder.Decode(&ret)
			ch <- ret
			return
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
	api := fmt.Sprintf("https://localhost:%d/api", port)
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
