bmi :: Float -> Float -> Float
bmi m h = m / (h*h)

bmiInterpretation :: Float -> String
bmiInterpretation bmi
	| bmi < 18				   = "underweight"
	| 18 <= bmi && bmi < 25    = "normal weight"
	| 25 <= bmi && bmi < 30    = "overweight"
	| 30 <= bmi && bmi < 40    = "obese"
	| 40 <= bmi 		       = "severely obese"

main :: IO()
main = do
	     l <- getLine
	     do
	     if (l /= "*") then
	     	do
	     		let [name, weight, height] = words l
	     		let b = bmi (read weight :: Float) (read height :: Float)
	     		let i = bmiInterpretation b
	     		putStr name
	     		putStr ": "
	     		putStrLn i
	     		main
	     else
	     	return ()
