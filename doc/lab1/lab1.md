# LAB 1: Context, Features and Priorities

This document provides instructions for a mandatory computer lab session in the course ETSN15 "Requirements Engineering" given at Lund University.

## Goals

To complete this lab you should develop a requirements model using [http://reqt.org/download][reqT] including the following sections:

  * **Context** including a context diagram for your course project product with some relevant interfaces to some relevant actors. The model does not have to be complete.

  * **Features** including high-level features from your course project, each with a descriptive id. Each feature should also have a Gist attribute with a short description of the feature.

  * **Priorities** including priorities for each feature based on the criteria: *which feature is most important to spend more elicitation effort on*.


## Background

About requirements models, example entities, attributes and relations etc.

## Preparations

On how to download and start reqT and some basics about the editor.

## Create a Context section

```
Section Context
  Product hotelApp interactsWith
    User receptionist
    User guest
    System accounting
  User guest interactsWith
    Product hotelApp
  User receptionist interactsWith
    Product hotelApp
  System telephony interactsWith
    Product hotelApp   
```

## Create a Features section  


## Prioritize Features
