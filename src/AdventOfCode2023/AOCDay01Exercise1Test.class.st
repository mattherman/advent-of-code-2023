Class {
	#name : #AOCDay01Exercise1Test,
	#superclass : #TestCase,
	#category : #AdventOfCode2023
}

{ #category : #tests }
AOCDay01Exercise1Test >> testExampleInput [
	| exercise |
	exercise := AOCDay01Exercise1 withInput: {
		'1abc2'.
		'pqr3stu8vwx'.
		'a1b2c3d4e5f'.
		'treb7uchet'.
	} .
	self assert: exercise solve equals: 142
]
