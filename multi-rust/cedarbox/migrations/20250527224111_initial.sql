-- SQLITE migration:

CREATE TABLE IF NOT EXISTS "organizations" (
    "id" TEXT PRIMARY KEY,
    "name" TEXT NOT NULL UNIQUE,
);

CREATE TABLE IF NOT EXISTS "users" (
    "id" TEXT PRIMARY KEY,
    "name" TEXT NOT NULL UNIQUE,
    "organization_id" TEXT NOT NULL,
    FOREIGN KEY ("organization_id") REFERENCES "organizations" ("id") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "schedules" (
    "id" TEXT PRIMARY KEY,
    "date" TEXT NOT NULL,
    "organization_id" TEXT NOT NULL,
    FOREIGN KEY ("organization_id") REFERENCES "organizations" ("id") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "jobs" (
    "id" TEXT PRIMARY KEY,
    "name" TEXT NOT NULL,
    "schedule_id" TEXT NOT NULL,
    FOREIGN KEY ("schedule_id") REFERENCES "schedules" ("id") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "roles" (
    "id" TEXT PRIMARY KEY,
    "name" TEXT NOT NULL UNIQUE,
    "organization_id" TEXT NOT NULL,
    FOREIGN KEY ("organization_id") REFERENCES "organizations" ("id") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "user_roles" (
    "user_id" TEXT NOT NULL,
    "role_id" TEXT NOT NULL,
    PRIMARY KEY ("user_id", "role_id"),
    FOREIGN KEY ("user_id") REFERENCES "users" ("id") ON DELETE CASCADE,
    FOREIGN KEY ("role_id") REFERENCES "roles" ("id") ON DELETE CASCADE
);

CREATE TABLE IF NOT EXISTS "role_settings" (
    "id" TEXT PRIMARY KEY,
    "role_id" TEXT NOT NULL,
    "effective_date" TEXT NOT NULL,

    "create_schedule" BOOLEAN NOT NULL DEFAULT 0,
    "edit_schedule" BOOLEAN NOT NULL DEFAULT 0,

    "create_user" BOOLEAN NOT NULL DEFAULT 0,
    "edit_user" BOOLEAN NOT NULL DEFAULT 0,

    "create_role" BOOLEAN NOT NULL DEFAULT 0,
    "edit_role" BOOLEAN NOT NULL DEFAULT 0,

    "create_organization" BOOLEAN NOT NULL DEFAULT 0,
    "edit_organization" BOOLEAN NOT NULL DEFAULT 0,

    "create_job" BOOLEAN NOT NULL DEFAULT 0,
    "edit_job" BOOLEAN NOT NULL DEFAULT 0,

    "cedar_policy" TEXT,
    FOREIGN KEY ("role_id") REFERENCES "roles" ("id") ON DELETE CASCADE
);