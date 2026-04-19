
Sets
    i locations / station1, station2/
    t hours /hour1, hour2, hour3, hour4, hour5, hour6, hour7, hour8, hour9, hour10, hour11, hour12, hour13, hour14, hour15, hour16, hour17, hour18, hour19, hour20, hour21, hour22, hour23, hour24/

Parameter
    k(i) /plant1   4, plant2   5/
    a(i) /plant1   0.8, plant2   1.1/
    b(i) /plant1   1e-2, plant2 2e-2/
    c(i) /plant1   -5e-4, plant2 -6e-4/
    inflow(i) /plant1 3, plant2 2/
    maxflow(i) /plant1 20, plant2 30/
    maxpower(i) /plant1 100, plant2 100/
    damcap(i) /plant1 1.5e6, plant2 1.8e5/
    maxspill(i) /plant1 50, plant2 50/
    maxchangerate(i) /plant1 0.25, plant2 0.25/
    intwl(i) /plant1 1e6, plant2 1e4/
    demand(t) /
        hour1   0,
        hour2   0,
        hour3   0,
        hour4   40,
        hour5   60,
        hour6   70,
        hour7   80,
        hour8   100,
        hour9   90,
        hour10  50,
        hour11  50,
        hour12  50,
        hour13  30,
        hour14  30,
        hour15  30,
        hour16  40,
        hour17  100,
        hour18  120,
        hour19  80,
        hour20  60,
        hour21  40,
        hour22  40,
        hour23  20,
        hour24  10
    /
    vw(i) /plant1 0.11, plant2 0.09/
    psell(t)
    pbuy(t);
    
    psell(t) = 39;
    pbuy(t)  = 44;
    
    psell(t)$(ord(t) >= 5 and ord(t) <= 7) = 45;
    pbuy(t)$(ord(t) >= 5 and ord(t) <= 7)  = 50;
;


Variable z;
Positive Variable
    turbineflow(i,t)
    spill(i, t)
    buy(t)
    sell(t)
    waterlevel(i, t)
    powergen(i, t)
;

Equations
    objfun
    conwlmax
    conspillmax
    conmaxflow
    conwl1
    conwl2
    satdem(t)     fullfillment of aggregate demand
    defpower
    maxchangedown
    maxchangeup
    conmaxpower
    conmaxbuy
    conmaxsell
;

conspillmax(i,t).. spill(i,t) =l= maxspill(i);
conwlmax(i,t).. waterlevel(i,t)=l=damcap(i);
conmaxflow(i,t).. turbineflow(i,t) =l= maxflow(i);
conmaxpower(i,t).. powergen(i,t) =l= maxpower(i);
conmaxbuy(t).. buy(t) =l= 100;
conmaxsell(t).. sell(t) =l= 100;
maxchangeup(i, t)$(ord(t) > 1).. turbineflow(i, t) - turbineflow(i, t-1) =l= maxchangerate(i) * maxflow(i);
maxchangedown(i, t)$(ord(t) > 1).. turbineflow(i, t-1) - turbineflow(i, t) =l= maxchangerate(i) * maxflow(i);


conwl1(t)..
    waterlevel("plant1",t)
        =e=
            waterlevel("plant1",t-1)$(ord(t) > 1) +
            intwl("plant1")$(ord(t) = 1) + 
            3600*(
                inflow("plant1") - turbineflow("plant1",t) - spill("plant1",t)
            );

conwl2(t)..
    waterlevel("plant2",t)
        =e= waterlevel("plant2",t-1)$(ord(t) > 1) +
            intwl("plant2")$(ord(t) = 1) + 
            3600*(
                inflow("plant2") +
                0.6* (turbineflow("plant1",t-3) + spill("plant1",t-3))$(ord(t) > 3) +
                0.4* (turbineflow("plant1",t-2) + spill("plant1",t-2))$(ord(t) > 2) -
                turbineflow("plant2",t) - spill("plant2",t)
            );
     
defpower(i,t)..  powergen(i, t) =e= k(i)*(a(i)+ b(i)*turbineflow(i,t) + c(i)*turbineflow(i,t)**2)*turbineflow(i,t) ;
satdem(t).. sum(i, powergen(i,t)) + buy(t) - sell(t) =e= demand(t);       
objfun.. z=e=sum(t, psell(t)*sell(t) - buy(t)*pbuy(t)) + sum(i, vw(i)*waterlevel(i, "hour24"));

model test /all/;

solve test using nlp maximizing z;

display turbineflow.l, buy.l, sell.l, spill.l, waterlevel.l;




    

                            
