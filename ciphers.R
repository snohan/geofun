library(tidyverse)

# Tistoner ----
ciphertext <-
  readr::read_file(
    "tistoner.txt"
  ) %>% 
  stringr::str_replace_all(
    "\\s",
    " "
  )

solve_tistoner_cipher <- function(ciphertext) {
  
  # To decipher a Tistoner cipher, replace the letter by 
  # looking ahead for odd numbered words, 
  # looking back for even numbered words,
  # the amount of letters the letter's place in the word.
  
  # Using the built-in constant "letters"
  
  # To run through the whole ciphertext
  ciphertext_counter <- 1
  
  # Count just the letters and not spaces
  actual_letter_counter <- 0
  
  # Count the word number, assuming the ciphertext starts without any space
  word_counter <- 1
  
  plaintext <- ""
  
  while (ciphertext_counter <= stringr::str_length(ciphertext)) {
    
    ciphertext_letter_to_check <-
      stringr::str_sub(
        ciphertext,
        ciphertext_counter,
        ciphertext_counter
        )
    
    if(ciphertext_letter_to_check == " ") {
      
      plaintext_letter <- " "
      word_counter <- word_counter + 1
      actual_letter_counter <- 0    
      
    }

    # Two different algorithms depending on word count parity 
    
    if(ciphertext_letter_to_check != " " &
       word_counter %% 2 != 0) {
      
      actual_letter_counter <- actual_letter_counter + 1
      
      plaintext_letter <- 
        letters[
          (which(letters == ciphertext_letter_to_check) + actual_letter_counter) %% 26
        ]
      
    }
     
    if(ciphertext_letter_to_check != " " &
       word_counter %% 2 == 0) {
      
      actual_letter_counter <- actual_letter_counter + 1
      
      plaintext_letter <- 
        letters[
          (which(letters == ciphertext_letter_to_check) - actual_letter_counter) %% 26
        ]
      
    }
    
    plaintext <- 
      paste0(
        plaintext,
        plaintext_letter
      )
    
    ciphertext_counter <- ciphertext_counter + 1
    
  }
  
  return(plaintext)
}

tistoner_cipher_solution <-
  ciphertext |>
  solve_tistoner_cipher()




