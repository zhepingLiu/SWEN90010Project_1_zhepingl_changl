# SWEN90010Project_1_zhepingl_changl
## Author Zheping Liu, Chang Lu

## Strategies for Implementation of ICD and CloseLoop

###In this paragraph we will introduce the strategies we devise for our implementation for ICD device.
###The ICD device mainly has two modes, which are modeOn and modeOff changed by  cardiologist and assistant. So we designed two functions called ‘On’ and ‘Off’ which can let ICD switch the mode between on and off once it checks that the operator is from a cardiologist or clinical assistant.
###However, the ICD should also be able to read and change current settings for assistant and cardiologist. So we designed two functions called ChangeSettingsResponse and ReadSettingsResponse, in which ICD can change and read its setting and send the response to assistant and cardiologist. Also, there is a function called ReadRateHistoryResponse, which can let assistant and cardiologist read patient’s heart rate history recorded in heart monitor.
###Finally, for ICD to detect whether there is a tachycardia or a ventricle fibrillation, we designed a function called isVentricleFibrillation to help to determine if there is a ventricle fibrillation.
###In the tick function, we will first record the rate and send it to heart monitor, then add the current rate to rate history. Then we will check if patient has a tachycardia, if so, we will record the current rate as a parameter and wait until the rate reached 15bpm above the current rate to set impluse generator to start sending 10*2 joules to heart. After checking tachycardia, we will check if there is a ventricle fibrillation using IsVentricleFibrilllation function.
