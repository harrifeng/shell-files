package main

import (
	"fmt"
	"net/http"
)

func main() {
	fmt.Println("Begin file server on 8080")
	http.ListenAndServe(":8080", http.FileServer(http.Dir(".")))
}
