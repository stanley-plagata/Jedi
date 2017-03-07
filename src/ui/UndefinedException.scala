package ui

class UndefinedException(val result: String = "Undefined values!") extends JediException(result) {}