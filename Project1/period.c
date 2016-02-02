//
//  main.c
//  Project 1
//
//  Created by Tyler Grimes on 1/31/16.
//
//  Code for binary tree obtained from:
//  http://www.thelearningpoint.net/computer-science/trees/binary-search-trees---c-program-source-code-and-documentation
//

#include<stdio.h>
#include<stdlib.h>
#include<math.h>

typedef struct treeNode
{
    int data;
    struct treeNode *left;
    struct treeNode *right;
    
}treeNode;

treeNode* FindMin(treeNode *node)
{
    if(node==NULL)
    {
        /* There is no element in the tree */
        return NULL;
    }
    if(node->left) /* Go to the left sub tree to find the min element */
        return FindMin(node->left);
    else
        return node;
}


treeNode * Insert(treeNode *node,int data)
{
    if(node==NULL)
    {
        treeNode *temp;
        temp = (treeNode *)malloc(sizeof(treeNode));
        temp -> data = data;
        temp -> left = temp -> right = NULL;
        return temp;
    }
    
    if(data >(node->data))
    {
        node->right = Insert(node->right,data);
    }
    else if(data < (node->data))
    {
        node->left = Insert(node->left,data);
    }
    /* Else there is nothing to do as the data is already in the tree. */
    return node;
    
}

treeNode * Delete(treeNode *node, int data)
{
    treeNode *temp;
    if(node==NULL)
    {
        printf("Element Not Found");
    }
    else if(data < node->data)
    {
        node->left = Delete(node->left, data);
    }
    else if(data > node->data)
    {
        node->right = Delete(node->right, data);
    }
    else
    {
        /* Now We can delete this node and replace with either minimum element
         in the right sub tree or maximum element in the left subtree */
        if(node->right && node->left)
        {
            /* Here we will replace with minimum element in the right sub tree */
            temp = FindMin(node->right);
            node -> data = temp->data;
            /* As we replaced it with some other node, we have to delete that node */
            node -> right = Delete(node->right,temp->data);
        }
        else
        {
            /* If there is only one or zero children then we can directly
             remove it from the tree and connect its parent to its child */
            temp = node;
            if(node->left == NULL)
                node = node->right;
            else if(node->right == NULL)
                node = node->left;
            free(temp); /* temp is longer required */
        }
    }
    return node;
    
}

void Free(treeNode *node) {
    if(node == NULL) {
        return;
    }
    else {
        Free(node->left);
        Free(node->right);
        free(node);
        return;
    }
}

treeNode * Find(treeNode *node, int data)
{
    if(node==NULL)
    {
        /* Element is not found */
        return NULL;
    }
    if(data > node->data)
    {
        /* Search in the right sub tree. */
        return Find(node->right,data);
    }
    else if(data < node->data)
    {
        /* Search in the left sub tree. */
        return Find(node->left,data);
    }
    else
    {
        /* Element Found */
        return node;
    }
}

void PrintInorder(treeNode *node)
{
    if(node==NULL)
    {
        return;
    }
    PrintInorder(node->left);
    printf("%d ",node->data);
    PrintInorder(node->right);
}


int nextRandom(int xn) {
    int a = (int) pow(7, 5);
    int c = (int) 0;
    int m = (int) pow(2, 31) - 1;
    return (int)(((long)a * (long)xn + c) % m);
}


int main(int argc, const char * argv[]) {
    
    //int seeds[] = {5612, 11453, 17062, 21276, 32001, 43455, 58869, 65537, 79302};
    int seeds[] = {1454324893, 1454336413, 1454362440, 1454371731, 1454372538, 1454373950, 1454381546, 1454386287};
    int k;
    for (k = 0; k < 9; k++) {
        int seed = seeds[k];
        int xn = seed;
        
        treeNode *root = NULL;
        treeNode *temp;
        root = Insert(root, seed);
        
        int REPEAT = 0;
        
        int i = 0;
        int MAX_REP = pow(10, 8);
        
        while (!REPEAT && i < MAX_REP) {
            i++;
            xn = nextRandom(xn);
            temp = Find(root, xn);
            if(temp == NULL) {
                root = Insert(root, xn);
            }
            else {
                printf("Seed: %d. Repeated value found. Period = %d\n", seed, i);
                REPEAT = 1;
            }
        }
        
        if (!REPEAT) {
            printf("Seed: %d. Period not found. MAX_REP = %d\n", seed, MAX_REP);
        }
        Free(root);
    }
    
    return 0;
}


