ALTER TABLE team
  ADD COLUMN hivy_id text NULL;

ALTER TABLE team
  ALTER COLUMN hivy_id SET STORAGE external;
