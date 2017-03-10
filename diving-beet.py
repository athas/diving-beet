#!/usr/bin/env python

import game
import numpy
import pygame
import time
import sys

beet = game.game(interactive=True)

fullscreen = False

if fullscreen:
    desired_size = None
else:
    desired_size = (1200,800)

pygame.init()
pygame.display.set_caption('Diving Beet')
if desired_size != None:
    screen = pygame.display.set_mode(desired_size)
else:
    screen = pygame.display.set_mode()
width = screen.get_width()
height = screen.get_height()
size = (width,height)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(100, 100)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

beet_state = beet.new_game(width, height)

def element_name(elem):
    return ''.join(map(chr,list(beet.element_name(elem).get())))

# Initialise the list of insertable elements and their names.
insertable = []
for elem in beet.insertable_elements().get():
    insertable += [(element_name(elem), elem)]
insertable.sort()
num_insertable = len(insertable)

selection = 0
# Set the initial selection to sand.
for i in range(num_insertable):
    if insertable[i][0] == 'sand':
        selection = i
        break

modify_radius = 5
old_pos = None

(ul_x, ul_y) = (0.0,0.0)
scale = 1.0
paused = False
pointing_at = None

def rebound():
    global ul_x, ul_y
    ul_x = min(1.0-scale, max(0, ul_x))
    ul_y = min(1.0-scale, max(0, ul_y))

def advance():
    global beet_state
    beet_state = beet.step(beet_state)

while True:
    start = time.time()
    if not paused:
        advance()
    frame = beet.render(beet_state, ul_x, ul_y, scale, width, height).get()
    end = time.time()
    futhark_time = (end-start)*1000

    start = time.time()
    pygame.surfarray.blit_array(screen, frame)
    end = time.time()
    blit_time = (end-start)*1000

    (mouse_x,mouse_y) = pygame.mouse.get_pos()
    if 0 <= mouse_x < width and 0 <= mouse_y < height:
        start = time.time()
        now_pointing_at = beet.element_at(beet_state,
                                          ul_x, ul_y, scale, width, height,
                                          mouse_x, mouse_y)
        if now_pointing_at != pointing_at:
            pointing_at = now_pointing_at
            pointing_at_name = element_name(now_pointing_at)
        end = time.time()
        query_time = (end-start)*1000
    else:
        query_time = 0

    speedmessage = "Stepping and rendering took %.2fms; blitting %.2fms; querying element name %.2fms" % \
                   (futhark_time, blit_time, query_time)

    showText(speedmessage, (10, 10))
    showText('%s (radius %d)' % (insertable[selection][0], modify_radius), (10,40))
    showText('Under cursor: %s' % pointing_at_name, (10,70))

    pygame.display.flip()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                modify_radius = min(modify_radius + 1, 100)
            if event.button == 5:
                modify_radius = max(modify_radius - 1, 1)
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_PAGEDOWN:
                selection = (selection + 1) % num_insertable
            elif event.key == pygame.K_PAGEUP:
                selection = (selection - 1) % num_insertable
            elif event.key == pygame.K_LEFT:
                ul_x -= 0.01
                rebound()
            elif event.key == pygame.K_RIGHT:
                ul_x += 0.01
                rebound()
            elif event.key == pygame.K_UP:
                ul_y -= 0.01
                rebound()
            elif event.key == pygame.K_DOWN:
                ul_y += 0.01
                rebound()
            elif event.unicode == '+':
                scale = max(1.0/min(width,height), scale*0.99)
                rebound()
            elif event.unicode == '-':
                scale = min(1.0, scale * 1.01)
                rebound()
            elif event.key == pygame.K_HOME:
                scale = 1.0
                (ul_x, ul_y) = (0.0, 0.0)
            elif event.key == pygame.K_SPACE:
                paused = True
                advance()
            elif event.key == pygame.K_RETURN:
                paused = False

    if pygame.mouse.get_pressed()[0] and pygame.mouse.get_pos() != None:
        # insert the selected element here.

        new_pos = pygame.mouse.get_pos()
        if old_pos == None:
            old_pos = new_pos

        args = [beet_state] + [ul_x, ul_y, scale, width, height] + list(old_pos) + list(new_pos) + [modify_radius, insertable[selection][1]]
        beet_state = beet.add_element(*args)
        old_pos = new_pos

    elif pygame.mouse.get_pressed()[2] and pygame.mouse.get_pos() != None:
        # remove any element element here.

        new_pos = pygame.mouse.get_pos()
        if old_pos == None:
            old_pos = new_pos

        args = [beet_state] + [ul_x, ul_y, scale, width, height] + list(old_pos) + list(new_pos) + [modify_radius]
        beet_state = beet.clear_element(*args)
        old_pos = new_pos
    else:
        old_pos = None
