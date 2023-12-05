Class {
	#name : #AOCDay01Exercise1,
	#superclass : #Object,
	#instVars : [
		'calibrationLines'
	],
	#category : #AdventOfCode2023
}

{ #category : #'as yet unclassified' }
AOCDay01Exercise1 class >> withInput: anArrayOfLines [
	^ self new calibrationLines: anArrayOfLines
]

{ #category : #accessing }
AOCDay01Exercise1 >> calibrationLines: anArrayofLines [
	calibrationLines := anArrayofLines 
]

{ #category : #initialization }
AOCDay01Exercise1 >> initialize [
	calibrationLines := { }
]

{ #category : #'as yet unclassified' }
AOCDay01Exercise1 >> solve [

	| nonEmptyLines digitList calibrationValues |
	nonEmptyLines := calibrationLines reject: [ :line | line isEmptyOrNil ].
	digitList := nonEmptyLines collect: [ :line | line select: #isDigit ].
	calibrationValues := digitList collect: [ :line |
		                     (line first asString , line last asString)
			                     asNumber ].
	^ calibrationValues sum
]
