// Encapsulate all the Futhark stuff.

package main

// #include "game.h"
// #include "stdlib.h"
// #cgo opencl pkg-config: OpenCL
// #cgo cuda LDFLAGS: -lcuda -lcudart -lnvrtc
// #cgo LDFLAGS: -lm
import "C"

import (
	"unsafe"
)

type Game struct {
	cfg       *C.struct_futhark_context_config
	ctx       *C.struct_futhark_context
	state     *C.struct_futhark_opaque_ext_game_state
	Frame     unsafe.Pointer
	screenX   C.int64_t
	screenY   C.int64_t
	nameCache map[C.uchar]string
}

type Element struct {
	code C.uchar
	Name string
}

func NewGame(screenX, screenY int32) Game {
	// Setup Futhark-side stuff.
	cfg := C.futhark_context_config_new()
	ctx := C.futhark_context_new(cfg)

	var state *C.struct_futhark_opaque_ext_game_state
	C.futhark_entry_new_game(ctx, &state, C.int64_t(screenX), C.int64_t(screenY))

	frame := C.malloc(C.ulong(screenX * screenY * 4))

	return Game{
		cfg, ctx, state, frame, C.int64_t(screenX), C.int64_t(screenY),
		make(map[C.uchar]string),
	}
}

func (g *Game) Free() {
	C.free(g.Frame)
	C.futhark_free_opaque_ext_game_state(g.ctx, g.state)
	C.futhark_context_free(g.ctx)
	C.futhark_context_config_free(g.cfg)
}

func (g *Game) Step() {
	old_state := g.state;
	defer C.futhark_free_opaque_ext_game_state(g.ctx, old_state)
	C.futhark_entry_step(g.ctx, &g.state, old_state)
}

func (g Game) Render(ul_x, ul_y, scale float64, screenX, screenY, mouse_x, mouse_y, radius int32) {
	var frame_fut *C.struct_futhark_u32_2d
	C.futhark_entry_render(g.ctx, &frame_fut, g.state,
		C.float(ul_x), C.float(ul_y), C.float(scale),
		C.int64_t(screenX), C.int64_t(screenY),
		C.int64_t(mouse_x), C.int64_t(mouse_y), C.int32_t(radius))
	defer C.futhark_free_u32_2d(g.ctx, frame_fut)
	C.futhark_values_u32_2d(g.ctx, frame_fut, (*C.uint32_t)(g.Frame))
	C.futhark_context_sync(g.ctx)
}

func (g *Game) AddElem(
	ul_x, ul_y, scale float64,
	from_x, from_y, to_x, to_y int32,
	radius int32, what Element) {
	defer C.futhark_free_opaque_ext_game_state(g.ctx, g.state)
	C.futhark_entry_add_element(
		g.ctx, &g.state, g.state,
		C.float(ul_x), C.float(ul_y), C.float(scale), C.int64_t(g.screenX), C.int64_t(g.screenY),
		C.int64_t(from_x), C.int64_t(from_y), C.int64_t(to_x), C.int64_t(to_y),
		C.int32_t(radius), C.uint8_t(what.code))
}

func (g *Game) ClearElem(
	ul_x, ul_y, scale float64,
	from_x, from_y, to_x, to_y int32,
	radius int32) {
	defer C.futhark_free_opaque_ext_game_state(g.ctx, g.state)
	C.futhark_entry_clear_element(
		g.ctx, &g.state, g.state,
		C.float(ul_x), C.float(ul_y), C.float(scale), C.int64_t(g.screenX), C.int64_t(g.screenY),
		C.int64_t(from_x), C.int64_t(from_y), C.int64_t(to_x), C.int64_t(to_y),
		C.int32_t(radius))
}

func (g Game) Elements() []Element {
	var elements_fut *C.struct_futhark_u8_1d

	C.futhark_entry_insertable_elements(g.ctx, &elements_fut)
	defer C.futhark_free_u8_1d(g.ctx, elements_fut)

	num_elements := *C.futhark_shape_u8_1d(g.ctx, elements_fut)

	elements := C.malloc(C.ulong(num_elements))
	defer C.free(elements)

	C.futhark_values_u8_1d(g.ctx, elements_fut, (*C.uchar)(elements))

	ret := make([]Element, num_elements)
	for i := 0; i < (int)(num_elements); i++ {
		element := *(*C.uchar)(unsafe.Pointer(((uintptr)(elements)) + (uintptr)(i)))

		var name_fut *C.struct_futhark_u8_1d

		C.futhark_entry_element_name(g.ctx, &name_fut, element)
		defer C.futhark_free_u8_1d(g.ctx, name_fut)

		num_chars := (int)(*C.futhark_shape_u8_1d(g.ctx, name_fut))

		name := C.malloc(C.ulong(num_chars))
		defer C.free(name)

		C.futhark_values_u8_1d(g.ctx, name_fut, (*C.uchar)(name))
		C.futhark_context_sync(g.ctx)

		ret[i] = Element{element, g.elementName(element)}
	}
	return ret
}

func (g Game) ElementAt(ul_x, ul_y, scale float64, x, y int32) Element {
	var element C.uchar
	C.futhark_entry_element_at(
		g.ctx, &element, g.state,
		C.float(ul_x), C.float(ul_y), C.float(scale), g.screenX, g.screenY,
		C.int64_t(x), C.int64_t(y))
	return Element{element, g.elementName(element)}
}

func (g Game) elementName(element C.uchar) string {
	if name, present := g.nameCache[element]; present {
		return name
	}

	var name_fut *C.struct_futhark_u8_1d

	C.futhark_entry_element_name(g.ctx, &name_fut, element)
	defer C.futhark_free_u8_1d(g.ctx, name_fut)

	num_chars := (int)(*C.futhark_shape_u8_1d(g.ctx, name_fut))

	name := C.malloc(C.ulong(num_chars))
	defer C.free(name)

	C.futhark_values_u8_1d(g.ctx, name_fut, (*C.uchar)(name))
	C.futhark_context_sync(g.ctx)

	s := ""
	for i := 0; i < num_chars; i++ {
		s += string((uint8)(*(*uint8)(unsafe.Pointer(((uintptr)(name)) + ((uintptr)(i))))))
	}

	g.nameCache[element] = s

	return s
}
