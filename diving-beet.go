package main

import (
	"fmt"
	"github.com/veandco/go-sdl2/sdl"
	"github.com/veandco/go-sdl2/ttf"
	"sort"
	"time"
)

// ByName implements sort.Interface for []Element based on
// the Name field.
type ByName []Element

func (a ByName) Len() int           { return len(a) }
func (a ByName) Swap(i, j int)      { a[i], a[j] = a[j], a[i] }
func (a ByName) Less(i, j int) bool { return a[i].Name < a[j].Name }

func min(x, y float64) float64 {
	if x < y {
		return x
	} else {
		return y
	}
}

func max(x, y float64) float64 {
	if x < y {
		return y
	} else {
		return x
	}
}

func main() {
	var err error

	screenX := 1024
	screenY := 768
	ul_x := 0.0
	ul_y := 0.0
	scale := 1.0
	radius := 10
	selected_element := 0
	mouse_down := false
	last_mouse_x := int32(0)
	last_mouse_y := int32(0)
	paused := false
	steps_per_frame := 10

	game := NewGame(screenX, screenY)
	defer game.Free()

	elements := game.Elements()
	sort.Sort(ByName(elements))

	if err := sdl.Init(sdl.INIT_EVERYTHING); err != nil {
		panic(err)
	}
	defer sdl.Quit()

	if err := ttf.Init(); err != nil {
		panic(err)
	}

	var font *ttf.Font
	text_size := 20
	if font, err = ttf.OpenFont("font.ttf", text_size); err != nil {
		panic(err)
	}
	defer font.Close()

	window, err := sdl.CreateWindow("test", sdl.WINDOWPOS_UNDEFINED, sdl.WINDOWPOS_UNDEFINED,
		int32(screenX), int32(screenY), sdl.WINDOW_SHOWN)
	if err != nil {
		panic(err)
	}
	defer window.Destroy()

	window_surface, err := window.GetSurface()
	if err != nil {
		panic(err)
	}

	frame_surface, err :=
		sdl.CreateRGBSurfaceFrom(game.Frame, int32(screenX), int32(screenY), 32, screenX*4, 0xFF0000, 0xFF00, 0xFF, 0x00000000)
	if err != nil {
		panic(err)
	}
	defer frame_surface.Free()

	white := sdl.Color{R: 255, G: 255, B: 255, A: 255}
	showText := func(s string, x, y int32) {
		var solid *sdl.Surface
		if solid, err = font.RenderUTF8Solid(s, white); err != nil {
			panic(err)
		}
		defer solid.Free()

		r := sdl.Rect{X: x, Y: y, W: 0, H: 0}
		if err = solid.Blit(nil, window_surface, &r); err != nil {
			panic(err)
		}
	}

	render := func() {
		start := time.Now()

		if !paused {
			for i := 0; i < steps_per_frame; i++ {
				game.Step()
			}
		}
		game.Render(ul_x, ul_y, scale, screenX, screenY)

		fut_time := time.Now().Sub(start)

		start = time.Now()
		if err = frame_surface.Blit(nil, window_surface, nil); err != nil {
			panic(err)
		}
		blit_time := time.Now().Sub(start)

		showText(
			fmt.Sprintf(
				"Futhark call took %.2fms; blitting took %.2fms.",
				fut_time.Seconds()*1000, blit_time.Seconds()*1000),
			0, 0)
		showText(
			fmt.Sprintf(
				"Inserting %s (radius %d)",
				elements[selected_element].Name, radius),
			0, int32(text_size+5))
		showText(
			fmt.Sprintf(
				"Under cursor %s", game.ElementAt(ul_x, ul_y, scale, last_mouse_x, last_mouse_y).Name),
			0, int32((text_size+5)*2))

		window.UpdateSurface()
	}

	rebound := func() {
		ul_x = min(1.0-scale, max(0, ul_x))
		ul_y = min(1.0-scale, max(0, ul_y))
	}

	onKeyboard := func(t sdl.KeyboardEvent) {
		if t.Type == sdl.KEYDOWN {
			switch t.Keysym.Sym {
			case sdl.K_RIGHT:
				ul_x += 0.01
				rebound()
			case sdl.K_LEFT:
				ul_x -= 0.01
				rebound()
			case sdl.K_UP:
				ul_y -= 0.01
				rebound()
			case sdl.K_DOWN:
				ul_y += 0.01
				rebound()
			case sdl.K_PLUS, sdl.K_p:
				scale = max(1/min(float64(screenX), float64(screenY)), scale*0.99)
				rebound()
			case sdl.K_MINUS, sdl.K_l:
				scale = min(1.0, scale*1.01)
				rebound()
			case sdl.K_SPACE:
				paused = true
				game.Step()
			case sdl.K_RETURN:
				paused = false

			case sdl.K_PAGEUP:
				selected_element = (selected_element + 1) % len(elements)
			case sdl.K_PAGEDOWN:
				selected_element = selected_element - 1
				if selected_element < 0 {
					selected_element = len(elements) - 1
				}
			}
		}
	}

	onMouseButton := func(t sdl.MouseButtonEvent) {
	}

	onMouseWheel := func(t sdl.MouseWheelEvent) {
		radius += int(t.Y)
		if radius < 1 {
			radius = 1
		}
	}

	checkMouse := func() {
		x, y, s := sdl.GetMouseState()
		if !mouse_down && s != 0 {
			mouse_down = true
			last_mouse_x = x
			last_mouse_y = y
		}

		if mouse_down {
			if s == 0 {
				mouse_down = false
			} else if (s & sdl.ButtonLMask()) != 0 {
				game.AddElem(ul_x, ul_y, scale, last_mouse_x, last_mouse_y, x, y, radius, elements[selected_element])
			} else if (s & sdl.ButtonRMask()) != 0 {
				game.ClearElem(ul_x, ul_y, scale, last_mouse_x, last_mouse_y, x, y, radius)
			}
		}

		last_mouse_x, last_mouse_y = x, y
	}

	running := true
	for running {
		render()

		checkMouse()

		for event := sdl.PollEvent(); event != nil; event = sdl.PollEvent() {
			switch t := event.(type) {
			case *sdl.QuitEvent:
				println("Quit")
				running = false
			case *sdl.KeyboardEvent:
				onKeyboard(*t)
			case *sdl.MouseButtonEvent:
				onMouseButton(*t)
			case *sdl.MouseWheelEvent:
				onMouseWheel(*t)
			}
		}
	}
}
