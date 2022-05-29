from airflow import DAG

from airflow.operators.bash_operator import BashOperator

import datetime as dt



#specify the arguments like configurations , how many retries for a failed task etc.
default_args={
    'owner':'me',
    'start_date': dt.datetime(2022,4,1),
    'retries':1,
    'retry_delay': dt.timedelta(minutes=5),
    }


#creation of the dag  based on the arguments provided and other metadata.
dag=DAG('assigment2', description='Assignment_Airflow_names',default_args=default_args, schedule_interval=dt.timedelta(seconds=5),)





#store the result of bash command (capitalized the first letter) and then push it forward using xcom push
task1 = BashOperator(

            task_id="task1.1",

            bash_command='firstName=ioannis &&''echo "${firstName}"',
            dag=dag,)


        
        


#store the result of bash command (capitalized the first letter) and then push it forward using xcom push
task2 = BashOperator(
        
                    task_id="task1.2",
        
                    bash_command='lastName=dekoulakos &&''echo "${lastName}"',
                    dag=dag,)
        
        
#take the output of task1 then store the result of bash command (capitalized the first letter) and then push it forward using xcom push       
       
task3 = BashOperator(
        
                    task_id="task2.1",
        
                    bash_command=f'firstNameCapitalized=$(echo {task1.output} | awk \'{{ $1=toupper(substr($1,1,1)) substr($1,2)}}1\') && '
                    'echo "${firstNameCapitalized}"',
                    dag=dag,)


#take the output of task2 then store the result of bash command (capitalized the first letter) and then push it forward using xcom push           
task4 = BashOperator(
        
                    task_id="task2.2",
        
                    bash_command=f'lastNameCapitalized=$(echo {task2.output} | awk \'{{ $1=toupper(substr($1,1,1)) substr($1,2)}}1\') && '
                    'echo "${lastNameCapitalized}"',
                    dag=dag,)
                    
 #take the result of task3 , task4  and print them                    
task5 = BashOperator(
        
                    task_id="task2.3",
                     do_xcom_push=False,
        
                    bash_command='echo "My Name is:"'
                    f'{task3.output} {task4.output}',
                    dag=dag,)




#Specifies the dependencies between the tasks

task1>>task3>>task5

task2>>task4>>task5





