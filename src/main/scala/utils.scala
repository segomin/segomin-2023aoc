package utils

import inputs.Input.loadFileSync
import locations.Directory.currentDir

def loadInput(filename: String): String = loadFileSync(s"$currentDir/input/$filename")
