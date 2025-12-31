// Ported from https://github.com/SimonWaldherr/FluidSimASCII to Go--
// Originally the famous C code by Yusuke Endoh

package main

const CONSOLE_WIDTH = 80
const CONSOLE_HEIGHT = 24

const INPUT_DATA = "     ### . . . . . . . . . . . ###\n     ###.......................###\n     ### . . . . . . . . . . . ###\n     ###.......................###\n     ### . . . . . . . . . . . ###\n     ###.......................###\n     ### . . . . . . . . . . . ###\n     ###.......................###\n     ### . . . . . . . . . . . ###\n     ###.......................###\n     ### . . . . . . . . . . . ###\n     ###.......................##\n     ### . . . . . . . . . . .       ##\n     ###.......................     ###\n     #################################               ##         ##\n      ###############################                ##         ##\n                                                     ##         ##\n                                                      ###########"

var xSandboxAreaScan int = 0
var ySandboxAreaScan int = 0

type Particle struct {
	XPos      float64
	YPos      float64
	Density   float64
	Wallflag  int
	XForce    float64
	YForce    float64
	XVelocity float64
	YVelocity float64
}

var particles [3840]Particle
var xParticleDistance float64
var yParticleDistance float64
var particlesInteraction float64
var particlesDistance float64
var x int
var y int
var screenBufferIndex int
var totalOfParticles int

var gravity int = 1
var pressure int = 4
var viscosity int = 7

var screenBuffer [1921]byte

// No math library, so we use Newton's method to approximate sqrt
func sqrt(x float64) float64 {
	if x == 0.0 {
		return 0.0
	}
	z := 1.0
	for i := 0; i < 10; i++ {
		z -= (z*z - x) / (2.0 * z)
	}
	return z
}

// Yeah, we don't have bitwise ops
func logicOr(val byte, bit byte) byte {
	v := int(val)
	b := int(bit)

	if b == 1 {
		if v%2 != 0 {
			return val
		}
		return byte(v + 1)
	}
	if b == 2 {
		if (v/2)%2 != 0 {
			return val
		}
		return byte(v + 2)
	}
	if b == 4 {
		if (v/4)%2 != 0 {
			return val
		}
		return byte(v + 4)
	}
	if b == 8 {
		if (v/8)%2 != 0 {
			return val
		}
		return byte(v + 8)
	}
	return val
}

func main() {
	println("\x1b[2J")

	var particlesCounter int = 0
	var inputIdx int = 0
	var inputLen int = len(INPUT_DATA)
	var char byte

	for inputIdx < inputLen {
		char = INPUT_DATA[inputIdx]

		if char == 10 {
			ySandboxAreaScan += 2
			xSandboxAreaScan = -1
		} else {
			if char == 32 {
			} else {
				if char == 35 {
					p := particles[particlesCounter+1].Wallflag
					particles[particlesCounter+1].Wallflag = 1
					particles[particlesCounter].Wallflag = p

					particles[particlesCounter].XPos = float64(xSandboxAreaScan)
					particles[particlesCounter].YPos = float64(ySandboxAreaScan)
					particles[particlesCounter+1].XPos = float64(xSandboxAreaScan)
					particles[particlesCounter+1].YPos = float64(ySandboxAreaScan + 1)
					particlesCounter += 2
					totalOfParticles = particlesCounter
				} else {
					particles[particlesCounter].XPos = float64(xSandboxAreaScan)
					particles[particlesCounter].YPos = float64(ySandboxAreaScan)
					particles[particlesCounter+1].XPos = float64(xSandboxAreaScan)
					particles[particlesCounter+1].YPos = float64(ySandboxAreaScan + 1)
					particlesCounter += 2
					totalOfParticles = particlesCounter
				}
			}
		}

		xSandboxAreaScan += 1
		inputIdx++
	}

	for {
		var particlesCursor int
		var particlesCursor2 int

		for particlesCursor = 0; particlesCursor < totalOfParticles; particlesCursor++ {
			particles[particlesCursor].Density = float64(particles[particlesCursor].Wallflag * 9)
			for particlesCursor2 = 0; particlesCursor2 < totalOfParticles; particlesCursor2++ {
				xParticleDistance = particles[particlesCursor].XPos - particles[particlesCursor2].XPos
				yParticleDistance = particles[particlesCursor].YPos - particles[particlesCursor2].YPos
				particlesDistance = sqrt(xParticleDistance*xParticleDistance + yParticleDistance*yParticleDistance)
				particlesInteraction = particlesDistance/2.0 - 1.0

				if particlesInteraction <= 0.0 {
					particles[particlesCursor].Density += particlesInteraction * particlesInteraction
				}
			}
		}

		for particlesCursor = 0; particlesCursor < totalOfParticles; particlesCursor++ {
			particles[particlesCursor].YForce = float64(gravity)
			particles[particlesCursor].XForce = 0.0
			for particlesCursor2 = 0; particlesCursor2 < totalOfParticles; particlesCursor2++ {
				xParticleDistance = particles[particlesCursor].XPos - particles[particlesCursor2].XPos
				yParticleDistance = particles[particlesCursor].YPos - particles[particlesCursor2].YPos
				particlesDistance = sqrt(xParticleDistance*xParticleDistance + yParticleDistance*yParticleDistance)
				particlesInteraction = particlesDistance/2.0 - 1.0

				if particlesInteraction <= 0.0 {
					particles[particlesCursor].XForce += particlesInteraction * (xParticleDistance*(3.0-particles[particlesCursor].Density-particles[particlesCursor2].Density)*float64(pressure) + particles[particlesCursor].XVelocity*float64(viscosity) - particles[particlesCursor2].XVelocity*float64(viscosity)) / particles[particlesCursor].Density
					particles[particlesCursor].YForce += particlesInteraction * (yParticleDistance*(3.0-particles[particlesCursor].Density-particles[particlesCursor2].Density)*float64(pressure) + particles[particlesCursor].YVelocity*float64(viscosity) - particles[particlesCursor2].YVelocity*float64(viscosity)) / particles[particlesCursor].Density
				}
			}
		}

		for screenBufferIndex = 0; screenBufferIndex < 1920; screenBufferIndex++ {
			screenBuffer[screenBufferIndex] = 0
		}

		for particlesCursor = 0; particlesCursor < totalOfParticles; particlesCursor++ {
			if particles[particlesCursor].Wallflag == 0 {
				if sqrt(particles[particlesCursor].XForce*particles[particlesCursor].XForce+particles[particlesCursor].YForce*particles[particlesCursor].YForce) < 4.2 {
					particles[particlesCursor].XVelocity += particles[particlesCursor].XForce / 10.0
					particles[particlesCursor].YVelocity += particles[particlesCursor].YForce / 10.0
				} else {
					particles[particlesCursor].XVelocity += particles[particlesCursor].XForce / 11.0
					particles[particlesCursor].YVelocity += particles[particlesCursor].YForce / 11.0
				}
				particles[particlesCursor].XPos += particles[particlesCursor].XVelocity
				particles[particlesCursor].YPos += particles[particlesCursor].YVelocity
			}
			x = int(particles[particlesCursor].XPos)
			y = int(particles[particlesCursor].YPos / 2.0)
			screenBufferIndex = x + CONSOLE_WIDTH*y
			if y >= 0 {
				if y < 23 {
					if x >= 0 {
						if x < 79 {
							screenBuffer[screenBufferIndex] = logicOr(screenBuffer[screenBufferIndex], 8)
							screenBuffer[screenBufferIndex+1] = logicOr(screenBuffer[screenBufferIndex+1], 4)
							screenBuffer[screenBufferIndex+CONSOLE_WIDTH] = logicOr(screenBuffer[screenBufferIndex+CONSOLE_WIDTH], 2)
							screenBuffer[screenBufferIndex+CONSOLE_WIDTH+1] = logicOr(screenBuffer[screenBufferIndex+CONSOLE_WIDTH+1], 1)
						}
					}
				}
			}
		}

		var outputString string = ""
		var lookupString string = " '`-.|//,\\|\\_\\/#"

		for screenBufferIndex = 0; screenBufferIndex < 1920; screenBufferIndex++ {
			if screenBufferIndex%CONSOLE_WIDTH == 79 {
				if screenBufferIndex != 1919 {
					outputString += "\n"
				}
			} else {
				var val byte = screenBuffer[screenBufferIndex]
				var charLookup byte = lookupString[val]
				outputString += string(charLookup)
			}
		}

		println("\x1b[1;1H" + outputString + "\x1b[1;1H")

		// The hacks we must use in place of a sleep function hahaha
		var busyWait int
		for busyWait = 0; busyWait < 6500000; busyWait++ {
			print("")
		}
	}
}
