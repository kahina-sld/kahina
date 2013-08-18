package org.kahina.logic.sat.insertionmus.algorithms;

import java.util.Iterator;
import java.util.NavigableSet;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.TimeoutException;

import org.kahina.logic.sat.io.minisat.FreezeFile;
import org.kahina.logic.sat.io.minisat.ResultNotRetrievableException;

public class BinaryAlgorithm extends AbstractAlgorithm{

	@Override
	public boolean nextStep(AlgorithmData data){
		try {
			if (solve(data, data.freezeM)){

				int min = 0;
				int max = data.instanceIDs.size();
				int mid = max/2;
				Iterator<Integer> it = data.instanceIDs.iterator();

//				System.out.println("S: min: " + min + " max: " + max + " mid: " + mid);
//				System.out.println(data.S);
//				System.out.println(data.M);
				for (int i = mid; i >= 0; i--){
					Integer id = it.next();
					data.S.add(id);
					data.freezeAll[id] = FreezeFile.UNFREEZE;
				}
				while (it.hasNext()){
					Integer id = it.next();
					data.freezeAll[id] = FreezeFile.FREEZE;
				}
				while (min != max){
//					System.out.println("1: min: " + min + " max: " + max + " mid: " + mid);
//					System.out.println(data.S);
					if (solve(data)){
						min = mid+1;
						mid = (min + max)/2;
						NavigableSet<Integer> addNew = data.instanceIDs.subSet(min, mid+1);
						data.S.addAll(addNew);

						for (Integer id: addNew){
							data.freezeAll[id] = FreezeFile.UNFREEZE;
						}
						System.out.println("min");
					}else{
						while (max > mid){
							max--;
							if (max > mid){
								max--;
								data.freezeAll[data.S.pollLast()] = FreezeFile.FREEZE;
							}
						}
						mid = (min + max)/2;
						System.out.println("max");
					}
//					System.out.println("2: min: " + min + " max: " + max + " mid: " + mid);
//					System.out.println(data.S);
				}
				System.out.println(data.S);
				int addToMus = data.S.pollLast();
				data.M.add(addToMus);
				data.freezeM[addToMus] = FreezeFile.UNFREEZE;
				data.instanceIDs = data.S;
				data.S = new ConcurrentSkipListSet<Integer>();

				System.out.println("3: min: " + min + " max: " + max + " mid: " + mid);
				System.out.println(data.M);
				System.out.print("[");
				for (int i: data.freezeM){

					System.out.print(i + ", ");
				}
				System.out.println("]");
			}else{
				data.isMus = true;
			}
		} catch (TimeoutException | ResultNotRetrievableException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return true;
	}

	@Override
	public boolean nextStep(int clauseIndex, AlgorithmData data) {
		//		throw new Exception();
		System.err.println("This function should not be used!");
		return false;
	}

}
