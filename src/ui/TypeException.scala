package ui

class TypeException(val result: String = "Type mismatch!") extends JediException(result) {}