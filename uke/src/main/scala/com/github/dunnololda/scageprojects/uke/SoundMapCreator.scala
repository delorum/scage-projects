package com.github.dunnololda.scageprojects.uke

import com.github.dunnololda.scage.ScageLib._
import org.newdawn.slick.openal.{AudioLoader, SoundStore}
import org.newdawn.slick.util.ResourceLoader

object SoundMapCreator extends ScageScreenApp("Sound Map Creator", 800, 600) {
  val oggStream = AudioLoader.getStreamingAudio("OGG", ResourceLoader.getResource("resources/sounds/badapple.ogg"))

  private var time = 0L
  def startPlaying() {
    time = 0L
    oggStream.playAsMusic(1.0f, 1.0f, true)
    pauseOff()
  }

  actionStaticPeriod(100) {
    time += 100
    SoundStore.get().poll(0)
  }

  backgroundColor = WHITE
  interface {
    print(time, windowWidth/2, windowHeight/2, BLACK)
  }

  key(KEY_SPACE, onKeyDown = {
    switchPause()
    time += 300
  })
  key(KEY_F2, onKeyDown = startPlaying())
}