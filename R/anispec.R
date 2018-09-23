#' Create moving spectrogram
#'
#' @keywords spectrogram, animation
#' @param sound_file Name of sound file
#' @param framerate Frame rate per second
#' @param output Output file, without the extension. All output will be *.avi.
#' @details Currently, only WAVE files are allowed as input. Only AVI files are
#' produced as output.
#' @export

anispec <- function(sound_file, framerate, output){
  # Open sound file----
  sound <- tuneR::readWave(filename = sound_file)

  # Generate frames----
  # Calculate the number of frames needed for the movie, based on a given
  # framerate (in fps).
  duration <- seewave::duration(sound)
  nframes <- floor(duration * framerate)

  # Create a function that generates the frames.
  create.frames <- function(){
    for (i in 1:nframes){
      seewave::spectro(sound, palette= seewave::reverse.gray.colors.1)
      abline(v = i / framerate)
    }
  }

  # Compile movie----
  # Compile the video
  animation::ani.options(interval= 1/framerate)
  intermediate_output <- paste(output, "mp4", sep= ".")
  animation::saveVideo(expr = create.frames(),
                       video.name = intermediate_output)

  # Add the audio
  final_output <- paste(output, "avi", sep= ".")
  if (file.exists(final_output)){
    print("Output file already exists. Cannot overwrite it.")
  }else{
    system2(command= "ffmpeg", args = paste(
      "-i", intermediate_output, "-i", sound_file, "-r", framerate,
      "-codec copy -shortest", final_output
    ))
  }
  file.remove(intermediate_output)
}
