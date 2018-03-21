SWEN90010 High Integrity Software System Project_1
Author Zheping Liu, Chang Lu

During the project, both of us read through the requirements and made
nots about the critical requirements to ensure the safety of the system.

After this, we discussed together to evaluate the conflict or missing
points in the notes of each other. For example, one confusion we had was about
tachycardia treatment, we had two different views on the requirements at
the beginning. 

During implementation, we worked on the same Github repository. Before we
start writing the our parts, we will first review the code uploaded by
each other. We checked the correctness of logic, we discuss about any confusions
and research online (including reading the Discussion Board).

After we completed the code, we wrote a small test program to test the activity
of the heart. During the testing we found heart rate went zero, we had to read 
both the requirements and the provided code again and again to trace the safety 
conditions we may miss. In this process, we first listed all conditions that
can lead to death of patient, then we read our code to find if any condition
was met. We compared the list to our implementation to check if any condition 
was met. One problem we found was Heart.SetImpulse would only be called inside
ImpulseGenerator.Tick procedure. After found this, we found several potential
hazards that are not mentioned in the specification but could lead to death of
patients.