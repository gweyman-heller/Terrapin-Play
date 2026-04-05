# Terrapin Play Dataset and Analysis

## Overview
This repository contains the full terrapin play dataset and a reduced dataset in which two sessions were removed. These excluded sessions deviated from the original sampling protocol because they were additional morning (AM) sessions conducted on the same day as another AM session, placing them outside the predefined temporal structure for morning observations.

The repository also includes corresponding R scripts for both datasets. The script associated with the reduced dataset focuses on the primary analyses and was used to assess whether the temporally clustered sessions influenced results. As results were consistent between the full and reduced datasets, the full dataset was retained for all final analyses.

## Data Structure
Each row represents a single play sequence.

A **play sequence** is defined as one or more completed approach–current riding circuits (bouts) separated by no more than 30 seconds. If more than 30 seconds elapsed between bouts, the behavior was coded as a new sequence and recorded on a separate row.


## Datasets
- Terrapin Play Full Dataset.csv  
- Terrapin Play Reduced Dataset.csv  


## Code
- Terrapin Play Full Dataset Code.R  
- Terrapin Play Reduced Dataset.R  


## Column Descriptions

### date
Date on which the observation session occurred.

### observer
Initials of the observer who recorded the behavior.

### time_of_day
Indicates whether the observation session occurred in the morning or afternoon.  
- **AM** – Session started no later than 10:00 AM  
- **PM** – Session started after 3:00 PM  

### time
Clock time at which the play behavior was recorded.

### turtle_id
Unique identification number of the turtle engaging in the play behavior.

### entry_position
Approach direction of the turtle relative to the pump’s water current.  
- **Under** – Turtle approached from beneath the current (swam along the bottom of the viewing window and then upward into the current)  
- **Direct** – Turtle approached from open water and swam directly toward the pump (perpendicular approach)  
- **Wall** – Turtle approached by climbing the right-side wall adjacent to the pump before entering the current  

### entry_prox
Distance from the pump (start of the current) at which the turtle initiated the current riding sequence.  
- **Close** – ≤ 0.6 m  
- **Medium** – > 0.6 m to 1.2 m  
- **Far** – > 1.2 m to 1.8 m  

### bouts
Integer indicating the number of completed approach–current riding circuits within a single play sequence.

A **bout** is one complete approach and current riding circuit. Multiple bouts were considered part of the same sequence if separated by no more than 30 seconds. Intervals exceeding 30 seconds marked the beginning of a new sequence and were recorded separately.

### session
Integer indicating the observation session to which each record belongs.

Sessions were assigned sequentially in chronological order based on date, with earlier sessions receiving lower numbers. When multiple sessions occurred on the same day, the morning (AM) session was assigned the lower session number. In the two instances where multiple AM sessions occurred on the same day in the full dataset, session numbers were assigned based on the order in which those sessions were conducted.

