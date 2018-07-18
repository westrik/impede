# impede

A distributed<sup>\*</sup> functional physically-based<sup>\*</sup> renderer.

<sub><sup>\*</sup> workin' on it</sub>

## project goals

- Practice writing fast numerical Haskell (performance optimization, parallelism, SIMD primitives)
- Learn stuff about math and physically-based rendering
- Play around with Kubernetes/Docker, gRPC, and Elm (distributed rendering with web preview)

## to-do

- [ ] **Replicate millipede**
	- [x] Output to png
	- [ ] Simple rendering loop with matrix - set everything to black and output to hardcoded file
	- [ ] Colours, vectors, rays (simple blended colour demo)
	- [ ] Shapes (render sphere)
	- [ ] Camera, antialiasing
	- [ ] Materials (Lambertian, metal, dielectric)
	- [ ] DoF, make camera positionable
	- [ ] Replicate millipede's random sphere scene
- [ ] Acceleration structures
- [ ] Add additional shapes, materials
- [ ] More accurate integrators
- [ ] More accurate cameras
- [ ] Use spectral representations for colours instead of RGB
- [ ] Use SIMD in main rendering path
- [ ] Thoroughly test critical parts e.g. Monte Carlo (QuickCheck?)
- [ ] Multi-node support (AWS/Google Cloud)
- [ ] Hair
- [ ] Volumetrics (OpenVDB, integrate with future fluid simulator project?)