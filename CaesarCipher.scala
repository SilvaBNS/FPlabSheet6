object CaesarCipher {
  def main(args: Array[String]): Unit = {
    val inputText = scala.io.StdIn.readLine("Enter the text : ")
    print("Enter the number of shifts : ")
    val shift = scala.io.StdIn.readInt() // the number of positions each letter want to be shifted

    val encryptedText = cipher(inputText, shift, encrypt) //parameter caesarEncrypt is a function
    println("Encrypted Text: " + encryptedText)

    val decryptedText = cipher(encryptedText, shift, decrypt)
    println("Decrypted Text: " + decryptedText)
  }


  def encrypt(inputText: String, shift: Int): String = {
    val alphabetSize = 26

    inputText.map { char_Input =>               //map function is used to process each character of the string individually
      if (char_Input.isLetter) {                //to check whether a letter or not
        val isUpperCase = char_Input.isUpper
        val base = if (isUpperCase) 'A' else 'a'  //to calculate the position of the character in alphabet for shifting
        val shiftedChar = ((char_Input - base + shift + alphabetSize) % alphabetSize + base).toChar  // by % it ensures the result stays within the range of the alphabet(0-25)

        if (isUpperCase)
          shiftedChar
        else
          shiftedChar.toLower
      }
      else {
        char_Input
      }
    }
  }


  def decrypt(cipherText: String, shift: Int): String = {
    encrypt(cipherText, -shift) //call caesarEncrypt func with negative shift value
  }


  def cipher(text: String, shift: Int, func: (String, Int) => String): String = {
    func(text, shift)
  }

}
