mydat <- read.csv("C:\\Users\\onk3\\Documents\\event-model-data\\Period-1-testing.csv")
size=dim(mydat)[1]

#[4] DRIVING MOTOR VEHICLE ON HIGHWAY WITHOUT REQUIRED LICENSE AND AUTHORIZATION                         
#[6] DRIVING VEH. W/O ADEQUATE REAR REG. PLATE ILLUMINATION                                              
#[7] DRIVING VEHICLE ON HIGHWAY WITH SUSPENDED REGISTRATION                                              
#[8] FAILURE OF INDIVIDUAL DRIVING ON HIGHWAY TO DISPLAY LICENSE TO UNIFORMED POLICE ON DEMAND           
#[9] FAILURE OF LICENSEE TO NOTIFY ADMINISTRATION OF CHANGE OF ADDRESS WITHIN 30 DAYS                    
#[10] PERSON DRIVING MOTOR VEHICLE ON HIGHWAY OR PUBLIC USE PROPERTY ON SUSPENDED LICENSE AND PRIVILEGE   
#[11] PERSON DRIVING MOTOR VEHICLE WHILE LICENSE SUSPENDED UNDER TR 17-106, 26-204, 26-206, 27-103    
#[28] FAILURE TO DISPLAY REGISTRATION CARD UPON DEMAND BY POLICE OFFICER
#[19] DRIVING VEHICLE ON HIGHWAY WITHOUT CURRENT REGISTRATION PLATES AND VALIDATION TABS
#[13] DISPLAYING EXPIRED REGISTRATION PLATES ISSUED BY ANY STATE
#[26] FAILURE TO ATTACH VEHICLE REGISTRATION PLATES AT FRONT AND REAR 
#
#[3] DRIVER FAILURE TO OBEY PROPERLY PLACED TRAFFIC CONTROL DEVICE INSTRUCTIONS   
#[5] DRIVING VEH. ON HWY. W/O REQUIRED MINIMUM EQUIPMENT
#[12] STOP LIGHTS (*)                                                                                     
#[14] DRIVER CHANGING LANES WHEN UNSAFE                                                                   
#[15] DRIVER FAILURE TO STOP AT STEADY CIRCULAR RED SIGNAL                                                
#[16] DRIVER FAILURE TO STOP AT STOP SIGN LINE                                                            
#[18] DRIVER WRITING A TEXT MSG. WHILE OPER. VEH. IN TRAVEL PORTION OF HWY                                
#[20] DRIVING VEHICLE WHILE UNDER THE INFLUENCE OF ALCOHOL                                                
#[21] DRIVING WHILE IMPAIRED BY ALCOHOL                                                                   
#[22] EXCEEDING MAXIMUM SPEED: 39 MPH IN A POSTED 30 MPH ZONE                                             
#[23] EXCEEDING THE POSTED SPEED LIMIT OF 30 MPH                                                          
#[24] EXCEEDING THE POSTED SPEED LIMIT OF 40 MPH                                                          
#[25] FAILURE OF VEH. ON HWY. TO DISPLAY LIGHTED LAMPS, ILLUMINATING DEVICE IN UNFAVORABLE VISIBILITY COND
#[27] FAILURE TO CONTROL VEHICLE SPEED ON HIGHWAY TO AVOID COLLISION                                      
#[29] FAILURE TO DISPLAY TWO LIGHTED FRONT LAMPS WHEN REQUIRED                                            
#[30] FAILURE TO STOP AT STOP SIGN                                                                        
#[31] HEADLIGHTS (*)                                                                                      
#[32] NEGLIGENT DRIVING VEHICLE IN CARELESS AND IMPRUDENT MANNER ENDANGERING PROPERTY, LIFE AND PERSON 
#[33] OPER. MOTOR VEH. WITH OCCUPANT UNDER 16 NOT RESTRAINED BY SEAT BELT                                 
#[34] OPERATING VEHICLE ON HIGHWAY WITH UNAUTHORIZED WINDOW TINTING MATERIAL  
#[17] DRIVER FAILURE TO STOP FOR PEDESTRIAN IN CROSSWALK
#[35] PEDESTRIAN FAIL TO OBEY "DONT WALK", "UPRAISED HAND" SIGNAL

# choosen event - FAILURE TO CONTROL VEHICLE SPEED ON HIGHWAY TO AVOID COLLISION (25)
event1=mydat[,11]
event2=mydat[,7]
cor(event1, event2) 

#Since it is close to 1, we can conclude that the variables are positively linearly related.
event1=mydat[,3]
event2=mydat[,4]
cor(event1, event2) #0.3383378

event1=mydat[,4]
event2=mydat[,6]
cor(event1, event2) #0.4064288

event1=mydat[,4]
event2=mydat[,7]
cor(event1, event2) #0.3623195

event1=mydat[,9]
event2=mydat[,11]
cor(event1, event2) #0.8972553

event1=mydat[,7]
event2=mydat[,9]
cor(event1, event2) #0.400667

event1=mydat[,4]
event2=mydat[,9]
cor(event1, event2) #0.3162871

event1=mydat[,5]
event2=mydat[,12]
cor(event1, event2) #0.2787091
