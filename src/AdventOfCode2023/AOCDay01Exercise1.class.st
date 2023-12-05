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
	| calibrationValues |
	calibrationValues := calibrationLines
		collect: [ :line | ((line indexOf: (line findFirst: #isDigit)) , (line indexOf: (line findLast: #isDigit))) asNumber ] .
	^ calibrationValues sum
]
