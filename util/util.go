package util

import "testing"

// Why the hell doesn't Go have these!?

func Assert(t *testing.T, result bool) bool {
	if !result {
		t.Fatal("assertion failed")
		return false
	}
	return true
}

func AssertEqual[T comparable](t *testing.T, left T, right T) bool {
	if left != right {
		t.Fatalf("assertion failed: expected '%v', got '%v'.", right, left)
		return false
	}
	return true
}
