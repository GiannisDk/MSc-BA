#import necessary packages
from kafka.admin import KafkaAdminClient, NewTopic
from kafka import KafkaProducer
from kafka import KafkaConsumer
import json




#create a KafkaAdminClient class
admin_client=KafkaAdminClient(boostrap_servers="localhost:9092", client_id="assignment")


# define an empty topic list
topic_list=[]


#Use the new topic class create a topic 'clima'
new_topic=NewTopic(name="clima", num_partitions=2,replaction_factor=1)

topic_list.append(new_topic)



#Use the create_topics to create new topics
admin_client.create_topics(new_topics=topic_list)



#use KafkaProducer class for messages in JSON
producer =KafkaProducer(value_serializer=lambda v: json.dumps(v).encode('utf-8'))



#Create 5 climatic messeges
producer.send('clima', {'tempeture':20, 'humidity':0.4, 'timestamp':'2022-04-11T17:14:07Z'})

producer.send('clima', {'tempeture':21, 'humidity':0.5, 'timestamp':'2022-04-11T18:14:07Z'})
producer.send('clima', {'tempeture':23, 'humidity':0.56, 'timestamp':'2022-04-11T19:14:07Z'})
producer.send('clima', {'tempeture':22, 'humidity':0.6, 'timestamp':'2022-04-11T20:14:07Z'})
producer.send('clima', {'tempeture':24, 'humidity':0.45, 'timestamp':'2022-04-11T21:14:07Z'})



#Define & create a KafkaConsumer, subscribing to the topic 'clima'.
consumer = KafkaConsumer('clima')


#Iterate & print the messages
for msg in consumer:    print(msg.value.decode("utf-8"))