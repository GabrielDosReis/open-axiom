/*
  Copyright (C) 1991-2002, The Numerical Algorithms Group Ltd.
  All rights reserved.
  Copyright (C) 2007-2008, Gabriel Dos Reis.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are
  met:

      - Redistributions of source code must retain the above copyright
        notice, this list of conditions and the following disclaimer.

      - Redistributions in binary form must reproduce the above copyright
        notice, this list of conditions and the following disclaimer in
        the documentation and/or other materials provided with the
        distribution.

      - Neither the name of The Numerical Algorithms Group Ltd. nor the
        names of its contributors may be used to endorse or promote products
        derived from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#define _MSORT3D_C
#include "openaxiom-c-macros.h"


/*****************************************************
 * Mergesort routine                                 *
 *                                                   *
 * This file depends on the file msort.h. There, a   *
 * data type called linkElement is defined. It is    *
 * used here and is the main structure being sorted  *
 * here. You can sort any linked structure, under    *
 * any name - so long as it has a next field (see    *
 * below). The define statement, below, renames      *
 * linkElement to linkThing. All you need to do      *
 * is change the define statement to rename          *
 * your structure to linkThing. The first argument   *
 * you pass to the sort routine is a pointer to      *
 * the unsorted list. The function returns with      *
 * that same pointer pointing to a sorted list.      *
 *                                                   *
 *  Usage:                                           *
 *    linkElement *msort(p,min,max,compare)          *
 *      linkElement *L;                              *
 *      int min,max;                                 *
 *      int (*compare)();                            *
 *                                                   *
 *    e.g.                                           *
 *      msort(L,0,N,compare);                        *
 *                                                   *
 *    where                                          *
 *      L is the list of things to be sorted,        *
 *        it is expected to be a linked list         *
 *        where the following element is pointed     *
 *        to by a field called "next"                *
 *      0 is the index of the first element          *
 *        (since this routine is called recursively, *
 *        this field is kept for clarity; it will    *
 *        always be zero at top level)               *
 *      N the number of elements in the list         * 
 *        minus one                                  *
 *      compare(X,Y) is a comparison function that   *
 *        returns a -1 if X is less than Y           *
 *                   0 if X is the same as Y         *
 *               and 1 if X is greater than Y        *
 *                                                   *
 *****************************************************/


#include "header.h"

#include "all_3d.H1"


#define   linkThing poly



/**********************
 * merge(p,q,compare) *
 **********************/

linkThing *
merge(linkThing *p, linkThing *q,int (*compare)(linkThing *, linkThing *))
{
  linkThing *returnVal,*current,*pN,*qN;

  /* return if only one item - take out when insert sort implemented */
  if (!p) return(q); else if (!q) return(p);

    /* set up the head of the list (first element) */
  if (compare(p,q) <= 0) {
    returnVal = current = p;
    pN = p->next;
    qN = q;
  } else {
    returnVal = current = q;
    pN = p;
    qN = q->next;
  }

    /* merge the two lists */
  while ((pN != NULL) && (qN != NULL)) {
    if (compare(pN,qN) <= 0) {   /* pN <= qN */
      current->next = pN;
      current = pN;
      pN = pN->next;
    } else {
      current->next = qN;
      current = qN;
      qN = qN->next;
    }
  }

    /* tag on the tail end */
  if (pN == NULL) current->next = qN;
  else current->next = pN;

  return(returnVal);

} /* merge() */



/*********************************
 * msort: the top level function *
 *********************************/

linkThing *
msort(linkThing *p,int min,int max,int (*compare)(linkThing *, linkThing *))
{
  int mid;
  int i;
  linkThing *q,*temp,*xxx;

  if (min == max) return p;
  else {
    mid = (min + max - 1)/2;
       /* e.g. [min,max] = [1,6] => mid=3 => q points to 4th */
    for (i=min,q=p; i<mid; i++,q=q->next);
    temp = q->next;
    q->next = 0;
    xxx = merge(msort(p,min,mid,compare),
                msort(temp,mid+1,max,compare), compare);

    return(xxx); 
  }

} /* msort() */



