set search_path = 'multi_targeter';

drop table if exists campaigns cascade;
create table campaigns (
  id serial primary key,
  status text not null,
  name text not null,
  instructions text not null
);

drop table if exists targets cascade;
create table targets (
  id serial primary key,
  campaign_id int not null references campaigns(id),
  number text not null,
  active boolean not null default false,
  name text not null
);

drop table if exists callers cascade;
create table callers (
  id serial primary key,
  campaign_id int not null references campaigns(id),
  number text not null,
  created_at timestamp with time zone not null,
  ended_at timestamp with time zone,
  duration int,
  call_uuid text
);

drop table if exists calls cascade;
create table calls (
  id serial primary key,
  caller_id int not null references callers(id),
  target_id int not null references targets(id),
  created_at timestamp with time zone not null,
  status text,
  hangup_cause text,
  ended_at timestamp with time zone,
  duration int,
  call_uuid text,
  outcome text
);
