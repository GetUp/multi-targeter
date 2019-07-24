create table "public"."targets" (
  "id" serial
  , "name" text
  , "number" text
  , primary key ("id")
);

insert into "public"."targets" ("id" , "name" , "number")
    values (1 , E'Sydney office' , E'61285994347') , (2 , E'Melbourne office' , E'61285994347');
