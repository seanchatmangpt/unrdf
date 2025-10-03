package main

import (
	"fmt"

	"github.com/unrdf/knowd/internal/store"
)

func main() {
	q := store.Quad{Subject: "test", Predicate: "test", Object: "test", Graph: "test"}
	fmt.Printf("Quad: %+v\n", q)
}
