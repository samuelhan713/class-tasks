//Samuel Han
//samuel.han@stonybrook.edu

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#define N 50

struct entry {
    char name[N];
    char street[N];
    char city[N];
    char state[N];
    char zipcode[N];
    char phoneNumber[N];
    int id;
    char major[N];
    float gpa; 
    char applyingForScholarship[N];
    struct entry *next;
};

struct entry2 {
    char state[N];
    int count;
    struct entry2 *next;
};


typedef struct entry Node;
typedef struct entry2 Node2;


Node *studentRecords, *curr;
int numStudents;

//Problem a
void readApplications(char inputFile[]) {
    studentRecords = NULL;
    FILE *filename;

    if ((filename = fopen(inputFile, "r")) == NULL) {
        printf("Error: students.txt could not be opened\n");
    }

    fscanf(filename, "%d\n", &numStudents);
    while (!feof(filename)) {
        curr = (Node *)malloc(sizeof(Node));
        fscanf(filename, "%[^\n]%*c", curr->name);
        fscanf(filename, "%[^\n]%*c", curr->street);
        fscanf(filename, "%[^\n]%*c", curr->city);
        fscanf(filename, "%[^\n]%*c", curr->state);
        fscanf(filename, "%s\n", curr->zipcode);
        fscanf(filename, "%[^\n]%*c", curr->phoneNumber);
        fscanf(filename, "%d\n", &(curr->id));
        fscanf(filename, "%[^\n]%*c", curr->major); 
        fscanf(filename, "%f\n", &(curr->gpa));
        fscanf(filename, "%s\n", curr->applyingForScholarship);
        curr -> next = studentRecords;
        studentRecords = curr;
    }
}


//Problem b: Find scholarship recipients

void print(Node *studentRec) {
    curr = studentRec;
    while (curr) {
        printf("%s, %s, %s, %s, %s, %s, %d, %s, %f, %s\n\n", 
        curr->name, curr->street, curr->city, curr->state, curr->zipcode, curr->phoneNumber, curr->id, curr->major, curr->gpa, curr->applyingForScholarship);
        curr = curr->next;
    }
}


Node * findScholarshipRecipients(Node *studentRec) {
    Node *scholarshipRecipients;
    scholarshipRecipients = NULL;

    curr = studentRec;

    while (curr) {
        if ((curr->gpa > 3.8) && (strcmp(curr->major, "Philosophy")==0 || strcmp(curr->major, "History")==0 || strcmp(curr->major, "Computer Science")==0) && (strcmp(curr->state, "Montana")==0 || strcmp(curr->state, "North Dakota")==0) && (strcmp(curr->applyingForScholarship, "Applying4Scholarship")==0)) {
            Node *temp = (Node*)malloc(sizeof(Node));
            *temp = *curr;
            temp->next = scholarshipRecipients;
            scholarshipRecipients = temp;   
        }
        curr = curr -> next;
    }
    return scholarshipRecipients;
}



//Problem c
float averageGPA(char stateName[], Node *studentRec) {
    curr = studentRec;
    float sum = 0;
    int numStudents = 0;

    while (curr) {
        if (strcmp(curr->state, stateName) == 0) {
            sum += curr->gpa;    
            numStudents++;
        }
        curr = curr -> next;      
    }
    return sum / numStudents;
}



//Problem d
Node * highestGPA(Node *studentRec, char stateName[]) {
    curr = studentRec;
    float highestGPA = 0.0;
    Node *ret;
    
    while (curr) {
        if (strcmp(curr->state, stateName)==0) {
            if (curr->gpa > highestGPA) {
                highestGPA = curr->gpa;
                ret = curr;
            }
        }
        curr = curr -> next;
    }
    return ret;
}



//Problem e 
#define boolean int
#define true 1
#define false 0

void printArr(Node2 arr[], int size) {
    int i;
    for (i = 0; i < size; i++) {
        printf("%s, %d\n", arr[i].state, arr[i].count);
    }
}

int countOccurences(Node *studentRec, char state[]) {
    int count = 0;
    curr = studentRec;
    while (curr) {
        if (strcmp(curr->state, state) == 0) {
            count++;
        }
        curr = curr -> next;
    }
    return count;
}


void sort(Node2 arr[], int size) {
    int i;
    int j;
    for (i = 0; i < size; i++) {
        for (j = 0; j < size-1-i; j++) {
            if (strcmp(arr[j].state, arr[j+1].state) > 0) {
                Node2 temp = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = temp;
            }
        }
    }
}


boolean exists(Node2 arr[], char state[]) {
    int i;
    for (i = 0; i < numStudents; i++) {
        if (strcmp(arr[i].state, state) == 0) {
            return true;
        }
    }
    return false;
}

void histogram(Node *studentRec) { 
    curr = studentRec;
    Node2 *head2, *curr2;
    head2 = NULL;  
    

    int i = 0;
    while (curr) {
        curr2 = (Node2 *)malloc(sizeof(Node2));
        strcpy(curr2->state, curr->state);
        curr = curr -> next;
        curr2 -> next = head2;
        head2 = curr2;
        i++;
    }
    curr2 = head2;
    while (curr2) {
        curr2->count = countOccurences(studentRec, curr2->state);
        curr2 = curr2 -> next;
    }


    Node2 states[numStudents];
    curr2 = head2;
    
    int j = 0;
    while (curr2) {
        if (!exists(states, curr2->state)) {
            states[j] = *curr2;
            j++;
        }
        curr2 = curr2 -> next;
    }
    //adding the states to an array with the proper size
    Node2 actualStates[j];
    for (i = 0; i < j; i++) {
        actualStates[i] = states[i];
    }
    
    sort(actualStates, j);
    printArr(actualStates, j);
}



int main() {
    
    char filename[] = "a0.txt";
    //Problem a
    readApplications(filename);
    
    //Problem b
    printf("%s\n\n", findScholarshipRecipients(studentRecords));
    
    //Problem c
    char state[] = "California";
    printf("%f is the average gpa for %s residents!\n\n", state, averageGPA(state, studentRecords));
    
    //Problem d
    printf("%s with a GPA of %f\n\n", highestGPA(studentRecords, "Washington")->name, highestGPA(studentRecords, "Washington")->gpa);
    
    //Problem e
    histogram(studentRecords); //the histogram is printed in the histogram function
    return 0;
}