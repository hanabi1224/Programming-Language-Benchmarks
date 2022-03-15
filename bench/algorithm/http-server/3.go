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

	"github.com/evanphx/wildcat"
	"github.com/panjf2000/gnet"
)

var (
	client = http.Client{
		Timeout: 10 * time.Second,
	}
)

type httpServer struct {
	*gnet.EventServer
}

type httpCodec struct {
	parser *wildcat.HTTPParser
}

func (hs httpServer) OnOpened(c gnet.Conn) ([]byte, gnet.Action) {
	c.SetContext(&httpCodec{parser: wildcat.NewHTTPParser()})
	return nil, gnet.None
}

func (hs *httpServer) React(data []byte, c gnet.Conn) (out []byte, action gnet.Action) {
	hc := c.Context().(*httpCodec)

	headerOffset, err := hc.parser.Parse(data)
	if err != nil {
		return []byte("HTTP/1.1 500 Error\r\n\r\n"), gnet.Close
	}
	jsonBytes := data[headerOffset:]
	var req Payload
	if json.Unmarshal(jsonBytes, &req) != nil {
		return jsonBytes, gnet.Close
	}
	responseText := fmt.Sprintf("HTTP/1.1 200 OK\r\n\r\n%d\r\n", req.Value)
	return []byte(responseText), gnet.None
}

func runServer(port int) {
	http := new(httpServer)
	gnet.Serve(http, fmt.Sprintf("tcp://127.0.0.1:%d", port), gnet.WithMulticore(true))
}

func send(api string, value int, ch chan<- int) {
	ret := -1
	payload := Payload{Value: value}
	payloadBytes, _ := json.Marshal(payload)
	for {
		resp, err := client.Post(api, "", bytes.NewReader(payloadBytes))
		if err == nil {
			if resp.StatusCode == 200 {
				decoder := json.NewDecoder(resp.Body)
				decoder.Decode(&ret)
				ch <- ret
				return
			}
		} else {
			// println(err.Error())
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
