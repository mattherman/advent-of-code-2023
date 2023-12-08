Class {
	#name : #AOCDay01Exercise1Test,
	#superclass : #TestCase,
	#category : #'AdventOfCode2023-Day01Exercise1'
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

{ #category : #tests }
AOCDay01Exercise1Test >> testPuzzleInput [
	| input exercise |
	input := (FileLocator home / 'Development/aoc/2023/inputs/01.txt') contents lines.
	exercise := AOCDay01Exercise1 withInput: input.
	self assert: exercise solve equals: 54877
]
