package ui

class JediException(val gripe: String = "Error!") extends Exception(gripe) {}