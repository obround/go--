// RGB Mandelbrot in Go--

package main

const SCALE = 10000
const MAX_ITER = 40
const WIDTH = 100
const HEIGHT = 25

func mandelbrot(cx int, cy int) int {
	x := 0
	y := 0
	iter := 0

	for iter < MAX_ITER {
		x2 := (x * x) / SCALE
		y2 := (y * y) / SCALE

		if x2+y2 > 4*SCALE {
			return iter
		}
		y = (2*x*y)/SCALE + cy
		x = x2 - y2 + cx

		iter++
	}

	return iter
}

func printChar(v int) {
	if v >= MAX_ITER {
		print(" ")
	} else if v > 65 {
		print("\x1b[1;37m@")
	} else if v > 45 {
		print("\x1b[1;33m#")
	} else if v > 30 {
		print("\x1b[1;31m%")
	} else if v > 20 {
		print("\x1b[1;35m*")
	} else if v > 10 {
		print("\x1b[1;34m+")
	} else if v > 5 {
		print("\x1b[34m:")
	} else {
		print("\x1b[30;1m.")
	}
}

func main() {
	for py := 0; py < HEIGHT; py++ {
		cy := -12000 + (py*24000)/HEIGHT

		for px := 0; px < WIDTH; px++ {
			cx := -25000 + (px*35000)/WIDTH
			v := mandelbrot(cx, cy)
			printChar(v)
		}

		println("\x1b[0m")
	}
}
