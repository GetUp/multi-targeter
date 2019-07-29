drop table if exists campaigns;
create table campaigns (
  id int primary key,
  status text not null,
  name text not null,
  instructions text not null
);

drop table if exists targets;
create table targets (
  id int primary key,
  campaign_id int not null references campaigns(id),
  phone varchar(11),
  name text not null
);

drop table if exists callers;
create table callers (
  id int primary key,
  campaign_id int not null references campaigns(id),
  number text not null,
  created_at timestamp with time zone not null,
  ended_at timestamp with time zone,
  duration int,
  call_uuid text
);

drop table if exists calls;
create table calls (
  id int primary key,
  caller_id int not null references callers(id),
  target_id int not null references targets(id),
  created_at timestamp with time zone not null,
  ended_at timestamp with time zone,
  duration int,
  call_uuid text
);
