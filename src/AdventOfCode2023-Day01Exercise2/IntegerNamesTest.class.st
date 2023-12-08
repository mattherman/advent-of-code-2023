Class {
	#name : #IntegerNamesTest,
	#superclass : #TestCase,
	#category : #'AdventOfCode2023-Day01Exercise2'
}

{ #category : #tests }
IntegerNamesTest >> testReplaceAllIntegers [
	self assert: (IntegerNames replace: 'onetwothreefourfivesixseveneightnine') equals: 'o1et2ot3ef4rf5es6xs7ne8tn9e'
]

{ #category : #tests }
IntegerNamesTest >> testReplaceCombinedIntegerNames [
	self assert: (IntegerNames replace: 'abctwonedef') equals: 'abct2o1edef'
]

{ #category : #tests }
IntegerNamesTest >> testReplaceMixedIntegers [
	self assert: (IntegerNames replace: 'sometwoofthesesixarenineintegers') equals: 'somet2ooftheses6xaren9eintegers'
]

{ #category : #tests }
IntegerNamesTest >> testReplaceNoIntegers [
	self assert: (IntegerNames replace: 'somethingwithoutintegers') equals: 'somethingwithoutintegers'
]
