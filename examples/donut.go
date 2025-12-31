// Port of famous donut.c demo to Go--
// Original C code by Andy Sloane: https://www.a1k0n.net/2011/07/20/donut-math.html

package main

// We have to use trig tables because we don't have a math library
var sinTab [128]float64 = [128]float64{
	0.000, 0.049, 0.098, 0.147, 0.195, 0.243, 0.290, 0.337,
	0.383, 0.428, 0.471, 0.514, 0.556, 0.596, 0.634, 0.672,
	0.707, 0.741, 0.773, 0.803, 0.831, 0.858, 0.882, 0.904,
	0.924, 0.942, 0.957, 0.970, 0.981, 0.989, 0.995, 0.999,
	1.000, 0.999, 0.995, 0.989, 0.981, 0.970, 0.957, 0.942,
	0.924, 0.904, 0.882, 0.858, 0.831, 0.803, 0.773, 0.741,
	0.707, 0.672, 0.634, 0.596, 0.556, 0.514, 0.471, 0.428,
	0.383, 0.337, 0.290, 0.243, 0.195, 0.147, 0.098, 0.049,
	0.000, -0.049, -0.098, -0.147, -0.195, -0.243, -0.290, -0.337,
	-0.383, -0.428, -0.471, -0.514, -0.556, -0.596, -0.634, -0.672,
	-0.707, -0.741, -0.773, -0.803, -0.831, -0.858, -0.882, -0.904,
	-0.924, -0.942, -0.957, -0.970, -0.981, -0.989, -0.995, -0.999,
	-1.000, -0.999, -0.995, -0.989, -0.981, -0.970, -0.957, -0.942,
	-0.924, -0.904, -0.882, -0.858, -0.831, -0.803, -0.773, -0.741,
	-0.707, -0.672, -0.634, -0.596, -0.556, -0.514, -0.471, -0.428,
	-0.383, -0.337, -0.290, -0.243, -0.195, -0.147, -0.098, -0.049,
}

var cosTab [128]float64 = [128]float64{
	1.000, 0.999, 0.995, 0.989, 0.981, 0.970, 0.957, 0.942,
	0.924, 0.904, 0.882, 0.858, 0.831, 0.803, 0.773, 0.741,
	0.707, 0.672, 0.634, 0.596, 0.556, 0.514, 0.471, 0.428,
	0.383, 0.337, 0.290, 0.243, 0.195, 0.147, 0.098, 0.049,
	0.000, -0.049, -0.098, -0.147, -0.195, -0.243, -0.290, -0.337,
	-0.383, -0.428, -0.471, -0.514, -0.556, -0.596, -0.634, -0.672,
	-0.707, -0.741, -0.773, -0.803, -0.831, -0.858, -0.882, -0.904,
	-0.924, -0.942, -0.957, -0.970, -0.981, -0.989, -0.995, -0.999,
	-1.000, -0.999, -0.995, -0.989, -0.981, -0.970, -0.957, -0.942,
	-0.924, -0.904, -0.882, -0.858, -0.831, -0.803, -0.773, -0.741,
	-0.707, -0.672, -0.634, -0.596, -0.556, -0.514, -0.471, -0.428,
	-0.383, -0.337, -0.290, -0.243, -0.195, -0.147, -0.098, -0.049,
	0.000, 0.049, 0.098, 0.147, 0.195, 0.243, 0.290, 0.337,
	0.383, 0.428, 0.471, 0.514, 0.556, 0.596, 0.634, 0.672,
	0.707, 0.741, 0.773, 0.803, 0.831, 0.858, 0.882, 0.904,
	0.924, 0.942, 0.957, 0.970, 0.981, 0.989, 0.995, 0.999,
}

func sin(a float64) float64 {
	return sinTab[int(a*20.372)%128]
}

func cos(a float64) float64 {
	return cosTab[int(a*20.372)%128]
}

func main() {
	var A float64 = 0.0
	var B float64 = 0.0
	var i float64
	var j float64
	var zBuffer []float64
	var output []byte
	var frame []byte
	var chars string = ".,-~:;=!*#$@"
	var w int

	output = make([]byte, 1760)
	zBuffer = make([]float64, 1760)

	frame = make([]byte, 1785)
	frame[0] = 27
	frame[1] = 91
	frame[2] = 72

	var clear []byte
	clear = make([]byte, 4)
	clear[0] = 27
	clear[1] = 91
	clear[2] = 50
	clear[3] = 74
	println(string(clear))

	for {
		for w = 0; w < 1760; w++ {
			output[w] = 32
			zBuffer[w] = 0.0
		}

		for j = 0.0; j < 6.28; j = j + 0.07 {
			for i = 0.0; i < 6.28; i = i + 0.02 {
				var sinI float64 = sin(i)
				var cosI float64 = cos(i)
				var sinJ float64 = sin(j)
				var cosJ float64 = cos(j)
				var sinA float64 = sin(A)
				var cosA float64 = cos(A)
				var sinB float64 = sin(B)
				var cosB float64 = cos(B)

				var offset float64 = cosJ + 2.0
				var invDepth float64 = 1.0 / (sinI*offset*sinA + sinJ*cosA + 5.0)
				var t float64 = sinI*offset*cosA - sinJ*sinA

				var x int = int(40.0 + 30.0*invDepth*(cosI*offset*cosB-t*sinB))
				var y int = int(12.0 + 15.0*invDepth*(cosI*offset*sinB+t*cosB))
				var o int = x + 80*y

				var N int = int(8.0 * ((sinJ*sinA-sinI*cosJ*cosA)*cosB - sinI*cosJ*sinA - sinJ*cosA - cosI*cosJ*sinB))

				if y > 0 && y < 22 && x > 0 && x < 80 && invDepth > zBuffer[o] {
					zBuffer[o] = invDepth
					if N > 0 {
						output[o] = chars[N]
					} else {
						output[o] = 46
					}
				}
			}
		}

		var fIdx int = 3
		var r int
		var c int
		for r = 0; r < 22; r++ {
			for c = 0; c < 80; c++ {
				frame[fIdx] = output[r*80+c]
				fIdx++
			}
			frame[fIdx] = 10
			fIdx++
		}

		println(string(frame))

		A = A + 0.04
		B = B + 0.02

		// Sleep function hacks
		for w = 0; w < 10000000; w++ {
			print("")
		}
	}
}
