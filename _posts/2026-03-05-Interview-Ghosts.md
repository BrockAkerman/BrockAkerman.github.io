---
layout: post
title: "The SQL Interview Ghost"
date: 2026-03-05
categories: applied
tags: applied sql interview job-search
image: /assets/media/thumbs/BlogAssets/AI_Image/Interview_Ghost.jpg
excerpt: The SQL question that once haunted me.
---

A few months ago, I walked into what felt like a golden opportunity: an interview with a major fintech company in downtown Atlanta, just weeks after having been through a layoff. The phone screen with HR had gone great — friendly, encouraging, hopeful. I moved on to the technical round full of excitement.

Then came the whiteboard, the watchful eyes, and the pressure cooker.

I can write SQL queries and Python code all day long in the comfort of my Jupyter notebook. But put me on screenshare or MS Teams with anyone standing behind me watching every keystroke, and I crumble. The interviewer was genuinely kind and patient, but I still wasn’t prepared. I hadn’t done a live technical interview in over ten years, and I didn’t anticipate how much the setting would rattle me.

Months later, the question still nags at me — my own personal “Interview Ghost.” I finally sat down, removed the pressure, and solved it properly. Here’s roughly what they asked, and the correct solution.

### The Challenge: Flight Capacity Analysis

**Scenario**  
You work for a global airline. Leadership wants to identify “low-load” flights to optimize fuel efficiency and scheduling. They need a report showing exactly how many seats remain empty on every scheduled flight.

**Tables**

- `flights`
  - `flight_id` (PK): Unique flight identifier
  - `origin`: Departure city
  - `destination`: Arrival city
  - `plane_id` (FK): Assigned aircraft

- `planes`
  - `plane_id` (PK): Unique aircraft identifier
  - `total_seats`: Maximum passenger capacity

- `tickets`
  - `ticket_id` (PK): Unique ticket/booking identifier
  - `flight_id` (FK): The flight this ticket was sold for

**Task:** Write a SQL query that returns:

- `flight_id`
- `origin` 
- `destination`
- `empty_seats`

The Correct Solution:

```
SELECT
    f.flight_id,
    f.origin,
    f.destination,
    p.total_seats - COUNT(t.ticket_id) AS empty_seats
FROM flights AS f
INNER JOIN planes AS p
    ON f.plane_id = p.plane_id
LEFT JOIN tickets AS t
    ON f.flight_id = t.flight_id
GROUP BY
    f.flight_id,
    f.origin,
    f.destination,
    p.total_seats;
```

This is a trap question.  The keys are 1.) not falling for wrapping SUM() around p.total_seats and 2.) Using LEFT join on the ticket table join so as not to leave out the aircraft with no seats sold. 

It. Seems. So. Simple.  Why then are these questions so brutal live?  Is it the people watching over your shoulder? The fear of failing in real time? The massive stakes of the job itself? For me, it is probably a mixture of all three. As a statistician transitioning into data science, I live in Jupyter notebooks, markdown cells, and .py files. Live coding on a whiteboard or shared editor feels like an entirely different sport.  I still lean heavily on my tools, maybe I use them as crutches a little too much. I even built a .jsx decision-tree component for use with a Vite.js for model selection that takes the guess work out of analysis.  I will share that toy in a future post.  But interviews like this one remind me: preparation isn’t just knowing the syntax. It’s also knowing how to think clearly when your heart rate is 140 bpm. Come to think on it, even when I was employed and sitting comfortably on a very chemistry-cohesive team I never liked live coding even in front of people I was comfortable with either.

Well, anyway.  Here’s to finally laying my Interview Ghost to rest — or at least making peace with it.  Happy querying.