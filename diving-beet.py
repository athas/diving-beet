#!/usr/bin/env python

import game
import numpy
import pygame
import time
import sys

width=1200
height=800

size=(width,height)
beet = game.game()
beet_state = beet.new_game(height, width)

pygame.init()
pygame.display.set_caption('Cheap clone!')
screen = pygame.display.set_mode(size)
surface = pygame.Surface(size)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(1, 1)

while True:
    frame = beet.render(*beet_state).get()
    pygame.surfarray.blit_array(surface, frame)
    screen.blit(surface, (0, 0))

    pygame.display.flip()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
    time.sleep(0.1)
    print 'f'
    beet_state = beet.step_game(*beet_state)
