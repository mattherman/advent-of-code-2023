Class {
	#name : #IntegerNamesTest,
	#superclass : #TestCase,
	#category : #'AdventOfCode2023-Exercise01Day2'
}

{ #category : #tests }
IntegerNamesTest >> testReplaceAllIntegers [
	self assert: (IntegerNames replace: 'onetwothreefourfivesixseveneightnine') equals: '123456789'
]

{ #category : #tests }
IntegerNamesTest >> testReplaceCombinedIntegerNames [
	self assert: (IntegerNames replace: 'abctwonedef') equals: 'abc2nedef'
]

{ #category : #tests }
IntegerNamesTest >> testReplaceMixedIntegers [
	self assert: (IntegerNames replace: 'sometwoofthesesixarenineintegers') equals: 'some2ofthese6are9integers'
]

{ #category : #tests }
IntegerNamesTest >> testReplaceNoIntegers [
	self assert: (IntegerNames replace: 'somethingwithoutintegers') equals: 'somethingwithoutintegers'
]
