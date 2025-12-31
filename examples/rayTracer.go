// Generates ppm image of ray-traced scene; Use like this:
//     go-- rayTracer.go
//     ./rayTracer > image.ppm

package main

const WIDTH int = 4096
const HEIGHT int = 2048
const MAXDEPTH int = 60

type Vec struct {
	x float64
	y float64
	z float64
}

type Sphere struct {
	center  Vec
	radius  float64
	r       float64
	g       float64
	b       float64
	matType int
	refIdx  float64
}

func (v Vec) Add(o Vec) Vec {
	return Vec{x: v.x + o.x, y: v.y + o.y, z: v.z + o.z}
}

func (v Vec) Sub(o Vec) Vec {
	return Vec{x: v.x - o.x, y: v.y - o.y, z: v.z - o.z}
}

func (v Vec) Mul(t float64) Vec {
	return Vec{x: v.x * t, y: v.y * t, z: v.z * t}
}

func (v Vec) Dot(o Vec) float64 {
	return v.x*o.x + v.y*o.y + v.z*o.z
}

func (v Vec) Cross(o Vec) Vec {
	return Vec{
		x: v.y*o.z - v.z*o.y,
		y: v.z*o.x - v.x*o.z,
		z: v.x*o.y - v.y*o.x,
	}
}

func (v Vec) Len() float64 {
	return sqrt(v.x*v.x + v.y*v.y + v.z*v.z)
}

// No math library, so we use Newton's method to approximate sqrt
func sqrt(x float64) float64 {
	if x == 0.0 {
		return 0.0
	}
	z := 1.0
	for i := 0; i < 30; i++ {
		z -= (z*z - x) / (2.0 * z)
	}
	return z
}

func (v Vec) Unit() Vec {
	var l float64 = v.Len()
	return Vec{x: v.x / l, y: v.y / l, z: v.z / l}
}

func reflect(v Vec, n Vec) Vec {
	var d float64 = v.Dot(n)
	var twoN Vec = n.Mul(2.0 * d)
	return v.Sub(twoN)
}

func refract(uv Vec, n Vec, etaiOverEtat float64) Vec {
	var dt float64 = uv.Dot(n)
	var disc float64 = 1.0 - etaiOverEtat*etaiOverEtat*(1.0-dt*dt)
	if disc > 0.0 {
		var a Vec = uv.Sub(n.Mul(dt))
		a = a.Mul(etaiOverEtat)
		var b Vec = n.Mul(sqrt(disc))
		return a.Sub(b)
	}
	return Vec{x: 0.0, y: 0.0, z: 0.0}
}

func schlick(cosine float64, refIdx float64) float64 {
	var r0 float64 = (1.0 - refIdx) / (1.0 + refIdx)
	r0 = r0 * r0
	var c float64 = 1.0 - cosine
	var c5 float64 = c * c * c * c * c
	return r0 + (1.0-r0)*c5
}

func hitSphere(c Vec, r float64, rayOrg Vec, rayDir Vec) float64 {
	var oc Vec = rayOrg.Sub(c)
	var a float64 = rayDir.Dot(rayDir)
	var b float64 = 2.0 * oc.Dot(rayDir)
	var cc float64 = oc.Dot(oc) - r*r
	var disc float64 = b*b - 4.0*a*cc
	if disc < 0.0 {
		return -1.0
	}
	return (-b - sqrt(disc)) / (2.0 * a)
}

func trace(rayOrg Vec, rayDir Vec, spheres []Sphere, depth int) Vec {
	if depth <= 0 {
		return Vec{x: 0.0, y: 0.0, z: 0.0}
	}

	var closestT float64 = 100000.0
	var hitIdx int = -1

	var i int = 0
	for i < len(spheres) {
		var s Sphere = spheres[i]
		var t float64 = hitSphere(s.center, s.radius, rayOrg, rayDir)
		if t > 0.001 {
			if t < closestT {
				closestT = t
				hitIdx = i
			}
		}
		i++
	}

	if hitIdx == -1 {
		var unitDir Vec = rayDir.Unit()
		var t float64 = 0.5 * (unitDir.y + 1.0)
		var c1 Vec = Vec{x: 1.0, y: 1.0, z: 1.0}
		var c2 Vec = Vec{x: 0.5, y: 0.7, z: 1.0}
		c1 = c1.Mul(1.0 - t)
		c2 = c2.Mul(t)
		return c1.Add(c2)
	}

	var hitObj Sphere = spheres[hitIdx]
	var hitPos Vec = rayDir.Mul(closestT)
	hitPos = rayOrg.Add(hitPos)
	var N Vec = hitPos.Sub(hitObj.center)
	N = N.Unit()

	if hitObj.matType == 1 {
		var r Vec = reflect(rayDir, N)
		var reflectedCol Vec = trace(hitPos, r, spheres, depth-1)
		return Vec{x: reflectedCol.x * hitObj.r, y: reflectedCol.y * hitObj.g, z: reflectedCol.z * hitObj.b}
	}

	if hitObj.matType == 2 {
		var outwardNormal Vec
		var niOverNt float64
		var cosine float64

		if rayDir.Dot(N) > 0.0 {
			outwardNormal = N.Mul(-1.0)
			niOverNt = hitObj.refIdx
			cosine = hitObj.refIdx * rayDir.Dot(N) / rayDir.Len()
		} else {
			outwardNormal = N
			niOverNt = 1.0 / hitObj.refIdx
			cosine = -rayDir.Dot(N) / rayDir.Len()
		}

		var refracted Vec = refract(rayDir.Unit(), outwardNormal, niOverNt)
		var reflectProb float64 = 1.0

		if refracted.x != 0.0 || refracted.y != 0.0 || refracted.z != 0.0 {
			reflectProb = schlick(cosine, hitObj.refIdx)
		}

		var reflectCol Vec = trace(hitPos, reflect(rayDir, N), spheres, depth-1)
		var refractCol Vec = trace(hitPos, refracted, spheres, depth-1)

		var cRefl Vec = reflectCol.Mul(reflectProb)
		var cRefr Vec = refractCol.Mul(1.0 - reflectProb)

		var result Vec = cRefl.Add(cRefr)
		return Vec{x: result.x * hitObj.r, y: result.y * hitObj.g, z: result.z * hitObj.b}
	}

	var lightDir Vec = Vec{x: 1.0, y: 1.0, z: 0.5}
	lightDir = lightDir.Unit()

	var inShadow bool = false
	var shadowBias Vec = N.Mul(0.001)
	var shadowOrig Vec = hitPos.Add(shadowBias)

	var k int = 0
	for k < len(spheres) {
		if k != hitIdx {
			var sShadow Sphere = spheres[k]
			var tShadow float64 = hitSphere(sShadow.center, sShadow.radius, shadowOrig, lightDir)
			if tShadow > 0.001 {
				inShadow = true
				k = len(spheres)
			}
		}
		k++
	}

	var diff float64 = N.Dot(lightDir)
	if diff < 0.0 {
		diff = 0.0
	}
	if inShadow {
		diff = 0.0
	}

	var ambient float64 = 0.3
	var intensity float64 = ambient + diff*0.7

	return Vec{x: hitObj.r * intensity, y: hitObj.g * intensity, z: hitObj.b * intensity}
}

func main() {
	println("P3")
	println(WIDTH)
	println(HEIGHT)
	println(255)

	var spheres []Sphere = make([]Sphere, 0)

	spheres = append(spheres, Sphere{center: Vec{x: 0.0, y: -1000.0, z: 0.0}, radius: 1000.0, r: 0.5, g: 0.5, b: 0.5, matType: 0, refIdx: 0.0})

	spheres = append(spheres, Sphere{center: Vec{x: 0.0, y: 1.0, z: 0.0}, radius: 1.0, r: 1.0, g: 1.0, b: 1.0, matType: 2, refIdx: 1.5})
	spheres = append(spheres, Sphere{center: Vec{x: -2.1, y: 0.7, z: -0.5}, radius: 0.7, r: 0.8, g: 0.2, b: 0.2, matType: 0, refIdx: 0.0})
	spheres = append(spheres, Sphere{center: Vec{x: 2.1, y: 0.7, z: 0.5}, radius: 0.7, r: 0.8, g: 0.8, b: 0.8, matType: 1, refIdx: 0.0})

	spheres = append(spheres, Sphere{center: Vec{x: -0.8, y: 0.4, z: 1.5}, radius: 0.4, r: 0.3, g: 0.8, b: 0.3, matType: 0, refIdx: 0.0})
	spheres = append(spheres, Sphere{center: Vec{x: 1.0, y: 0.4, z: 2.0}, radius: 0.4, r: 0.9, g: 0.7, b: 0.2, matType: 1, refIdx: 0.0})
	spheres = append(spheres, Sphere{center: Vec{x: 0.5, y: 0.3, z: -1.0}, radius: 0.3, r: 0.6, g: 0.6, b: 0.9, matType: 2, refIdx: 1.5})

	var lookFrom Vec = Vec{x: 6.0, y: 2.0, z: 5.0}
	var lookAt Vec = Vec{x: 0.0, y: 0.5, z: 0.0}
	var vUp Vec = Vec{x: 0.0, y: 1.0, z: 0.0}
	var distToFocus float64 = 8.0

	var w Vec = lookFrom.Sub(lookAt)
	w = w.Unit()
	var u Vec = vUp.Cross(w)
	u = u.Unit()
	var v Vec = w.Cross(u)

	var aspect float64 = float64(WIDTH) / float64(HEIGHT)
	var viewportH float64 = 2.0 * 0.2
	var viewportW float64 = viewportH * aspect

	var horizontal Vec = u.Mul(viewportW * distToFocus)
	var vertical Vec = v.Mul(viewportH * distToFocus)

	var lowerLeft Vec = lookFrom.Sub(horizontal.Mul(0.5))
	lowerLeft = lowerLeft.Sub(vertical.Mul(0.5))
	lowerLeft = lowerLeft.Sub(w.Mul(distToFocus))

	var j int = HEIGHT - 1
	for j >= 0 {
		var i int = 0
		for i < WIDTH {
			var ui float64 = float64(i) / float64(WIDTH)
			var vi float64 = float64(j) / float64(HEIGHT)

			var hOffset Vec = horizontal.Mul(ui)
			var vOffset Vec = vertical.Mul(vi)
			var target Vec = lowerLeft.Add(hOffset)
			target = target.Add(vOffset)
			var dir Vec = target.Sub(lookFrom)

			var col Vec = trace(lookFrom, dir, spheres, MAXDEPTH)

			var ir int = int(col.x * 255.99)
			var ig int = int(col.y * 255.99)
			var ib int = int(col.z * 255.99)

			if ir > 255 {
				ir = 255
			}
			if ig > 255 {
				ig = 255
			}
			if ib > 255 {
				ib = 255
			}

			println(ir)
			println(ig)
			println(ib)

			i++
		}
		j--
	}
}
