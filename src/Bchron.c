// This code has some functions for the main Bchronology R function

#include<R.h>
#include<Rmath.h>
#include <Rinternals.h>

void rtruncn(double *a, double *b, double *x) {
  double A, B;
  double maxA, maxB, maxR, r2, r, th, u, accept=0.0;
  A = atan(*a);
  B = atan(*b);
  maxA = exp(-pow(*a,2)/4)/cos(A);
  maxB = exp(-pow(*b,2)/4)/cos(B);
  maxR = fmax2(maxA, maxB);
  if((*a<1) && (*b>-1)) maxR = exp(-0.25)*sqrt(2.0);
  while (accept==0) {
    r2 = runif(0.0,1.0);
    r = sqrt(r2)*maxR;
    th = runif(A,B);
    u = r*cos(th);
    *x = tan(th);
    accept = ((pow(*x,2)) < (log(u)*-4));
  }
}

void truncatedWalk(double *old, double *sd, double *low, double *high, double *newvalue) {
  double lowlimold, upplimold, y;
  lowlimold = (*low - *old)/ *sd;
  upplimold = (*high - *old)/ *sd;
  rtruncn(&lowlimold, &upplimold, &y);
  *newvalue = *old + *sd*y;
}

void truncatedRat(double *old, double *sd, double *low, double *high, double *newvalue, double *ratio) {
  double lowlimold, upplimold, lowlimnew, upplimnew, plowold, puppold, plownew, puppnew;
  lowlimold = (*low - *old)/ *sd;
  upplimold = (*high - *old)/ *sd;
  lowlimnew = (*low - *newvalue)/ *sd;
  upplimnew = (*high - *newvalue)/ *sd;
  plowold = pnorm(lowlimold,0.0,1.0,1,0);
  puppold = pnorm(upplimold,0.0,1.0,1,0);
  plownew = pnorm(lowlimnew,0.0,1.0,1,0);
  puppnew = pnorm(upplimnew,0.0,1.0,1,0);
  *ratio = (puppold - plowold)/(puppnew - plownew);
}

void dtweedielogwsmallp(double *y, double *phi, double *power, double *logw) {
  double p,a,a1,r,drop=37,logz,jmax,j,cc,wmax,estlogw;
  int hij,lowj;

  if (*power < 1) error("Error - power<1!");
  if (*power > 2) error("Error - power>2!");
  if (*phi <= 0) error("Error - phi<=0!");
  if (*y <= 0) error("Error - y<=0!");
  p = *power;
  a = (2 - p)/(1 - p);
  a1 = 1 - a;
  r = -a * log(*y) + a * log(p - 1) - a1 * log(*phi) - log(2 - p);
  logz = r;

  jmax = (pow(*y,(2 - p)))/(*phi * (2 - p));
  j = fmax2(1, jmax);
  cc = logz + a1 + a * log(-a);
  wmax = a1 * jmax;
  estlogw = wmax;
  while (estlogw > (wmax - drop)) {
    j = j + 2;
    estlogw = j * (cc - a1 * log(j));
  }

  hij = (int)ceil(j);
  logz = r;
  jmax = pow(*y,(2 - *power))/(*phi * (2 - *power));
  j = fmax2(1, jmax);
  wmax = a1 * jmax;
  estlogw = wmax;
  while ((estlogw > (wmax - drop)) && (j >= 2)) {
    j = fmax2(1, j - 2);
    estlogw = j * (cc - a1 * log(j));
  }
  lowj = (int)fmax2(1, floor(j));

  double newj[hij-lowj+1];
  int k;
  for(k=0;k<(hij-lowj+1);k++) newj[k] = lowj+k;

  double g[hij-lowj+1];
  for(k=0;k<hij-lowj+1;k++) g[k] = lgamma(newj[k]+1)+lgamma(-a*newj[k]);

  double A[hij-lowj+1];
  for(k=0;k<hij-lowj+1;k++) A[k] = r*(double)newj[k]-g[k];

  double m=fmax2(A[0],hij-lowj+1);
  for(k=0;k<(hij-lowj+1);k++) m = fmax2(A[k],hij-lowj+1);

  double we[hij-lowj+1];
  for(k=0;k<hij-lowj+1;k++) we[k] = exp(A[k]-m);
  double sumwe=0;
  for(k=0;k<hij-lowj+1;k++) sumwe+=we[k];
  *logw=log(sumwe)+m;
}

void dtweedieseriessmallp(double *power, double *y, double *mu, double *phi, double *f) {
  double logw;
  dtweedielogwsmallp(y,phi,power,&logw);
  double tau = *phi*(*power-1)*pow(*mu,*power-1);
  double lambda = pow(*mu,2-*power)/(*phi*(2-*power));
  double logf = -*y/tau-lambda-log(*y)+logw;
  *f = exp(logf);
}

void dtweediep1(double *y, double *power, double *mu, double *phi, double *fTplus) {
  // Calculates the density of a tweedie plus one random variable
  double eps = 0.00000001;
  //double eps = 0.001;
  double lambda2 = pow(*mu,2-*power)/(*phi*(2-*power))-eps;
  double alpha = (2-*power)/(*power-1);
  double beta = 1/(*phi*(*power-1)*pow(*mu,*power-1));

  double mu2 = alpha*lambda2/beta;
  double phi2 = (alpha+1)/(pow(lambda2*alpha,(1/(alpha+1)))*pow(beta,(alpha/(alpha+1))));

  double fTplus1,fTplus2,fTplus3;
  dtweedieseriessmallp(power,y,mu,phi,&fTplus1);
  dtweedieseriessmallp(power,y,mu,phi,&fTplus2);
  dtweedieseriessmallp(power,y,&mu2,&phi2,&fTplus3);
  *fTplus = fTplus1+(1/eps)*(fTplus2-fTplus3);
}

void linInterp(int *n, double *newx, double *x, double *y, double *ans) {
  // Try to predict the value newx from the vectors x and y
  for(int i=0; i<*n-1; i++) {
    if(((*newx >= x[i]) & (*newx <= x[i+1])) | ((*newx <= x[i]) & (*newx >= x[i+1]))) {
      *ans = y[i] + ((*newx-x[i])/(x[i+1]-x[i]))*(y[i+1]-y[i]);
      if(*newx==x[i]) *ans = y[i];
    }
  }
}

void predictInterp(double *alpha, double *lambda, double *beta, double *predictPositions, int *NpredictPositions, double *diffPositionj, double *currPositionsj, double *currPositionsjp1, double *thetaj, double *thetajp1, double *predvals) {
  // Runs the prediction code when we are interpolating between two positions
  int Nd = rpois((*lambda)*(*diffPositionj));
  int i;
  // Watch out if Nd = 0 - just a straight linear interpolations
  int Ndmax = fmax2(1,Nd);
  double depthEvents[Ndmax];
  if(Nd>0) {
    for(i=0;i<Nd;i++) depthEvents[i] = runif(*currPositionsj,*currPositionsjp1);
    R_rsort(depthEvents,Nd);
  }
  double timeEventsUnsc[Nd+1],timeEventsSum=0.0;
  for(i=0;i<Nd+1;i++) timeEventsUnsc[i] = rgamma(*alpha,1/(*beta));
  for(i=0;i<Nd+1;i++) timeEventsSum += timeEventsUnsc[i];
  double timeEvents[Nd+1];
  for(i=0;i<Nd+1;i++) timeEvents[i] = (*thetajp1-*thetaj)*timeEventsUnsc[i]/timeEventsSum;
  double timeEventsCumsum[Nd+1],allTimeEvents[Nd+2];
  timeEventsCumsum[0] = 0.0;
  for(i=1;i<Nd+1;i++) timeEventsCumsum[i] = timeEventsCumsum[i-1] + timeEvents[i];
  for(i=0;i<Nd+1;i++) allTimeEvents[i] = timeEventsCumsum[i]+*thetaj;
  allTimeEvents[Nd+1] = *thetajp1;
  int Ndp2 = Nd+2;
  double allDepthEvents[Ndp2];
  allDepthEvents[0] = *currPositionsj;
  allDepthEvents[Nd+1] = *currPositionsjp1;
  if(Nd>0) {
    for(i=1;i<Nd+1;i++) allDepthEvents[i] = depthEvents[i-1];
  }
  for(i=0;i<*NpredictPositions;i++) {
    linInterp(&Ndp2,&predictPositions[i],allDepthEvents,allTimeEvents,&predvals[i]);
  }
}


void predictExtrapUp(double *alpha, double *lambda, double *beta, double *predictPositions, int *NpredictPositions, double *currPositions1, double *theta1, int *maxExtrap, double *extractDate, double *predvals) {
  // Runs the prediction code when we are extrapolating up beyond the first date
  int bad=1,count=0,i;
  double depthEvents[*maxExtrap],timeEvents[*maxExtrap];
  depthEvents[0] = *currPositions1;
  timeEvents[0] = *theta1;
  while(bad==1) {
    for(i=1;i<*maxExtrap;i++) {
      depthEvents[i] =  depthEvents[i-1]-rexp(1/(*lambda));
      timeEvents[i] =  timeEvents[i-1]-rgamma(*alpha,1/(*beta));
    }
    for(i=0;i<*NpredictPositions;i++) {
      linInterp(maxExtrap,&predictPositions[i],depthEvents,timeEvents,&predvals[i]);
    }
    count+=1;
    bad=0;
    for(i=0;i<*NpredictPositions;i++) {
      if(predvals[i]<*extractDate) bad=1;
    }
    if(count==50) {
      for(i=0;i<*NpredictPositions;i++) {
        if(predvals[i]<*extractDate) predvals[i] = *extractDate;
      }
      bad=0;
      warning("Unable to find suitable chronologies for top of core - truncated to date of extraction");
    }
  }
}

void predictExtrapDown(double *alpha, double *lambda, double *beta, double *predictPositions, int *NpredictPositions, double *currPositionsn, double *thetan, int *maxExtrap, double *predvals) {
  // Runs the prediction code when we are extrapolating down below the bottom date
  double depthEvents[*maxExtrap],timeEvents[*maxExtrap];
  int i;
  depthEvents[0] = *currPositionsn;
  timeEvents[0] = *thetan;
  for(i=1;i<*maxExtrap;i++) {
    depthEvents[i] =  depthEvents[i-1]+rexp(1/(*lambda));
    timeEvents[i] =  timeEvents[i-1]+rgamma(*alpha,1/(*beta));
  }
  for(i=0;i<*NpredictPositions;i++) {
    linInterp(maxExtrap,&predictPositions[i],depthEvents,timeEvents,&predvals[i]);
  }
}
