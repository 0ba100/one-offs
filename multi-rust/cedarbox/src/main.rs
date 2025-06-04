use std::str::FromStr;
use anyhow::Context;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let temporary_file = tempfile::NamedTempFile::new().context("Failed to create temp file")?;
    let path = temporary_file.path().to_str().context("invalid temporary file")?;
    let pool = sqlx::SqlitePool::connect(path).await.context("Failed to init sqlite db")?;

    sqlx::migrate!("./migrations")
        .run(&pool)
        .await
        .context("Failed to run migrations")?;

    seed_database(&pool).await.context("Failed to seed database")?;

    test_cedar();

    Ok(())
}

async fn seed_database(pool: &sqlx::SqlitePool) -> anyhow::Result<()> {
    // add an org
    sqlx::query("INSERT INTO organizations (id, name) VALUES (?, 'Main Organization')")
        .bind::<String>(uuid::Uuid::new_v4().into())
        .execute(pool)
        .await
        .context("Failed to insert organization")?;

    sqlx::query("INSERT INTO users (id, name, organization_id) VALUES (?, 'Alice', ?), (?, 'Bob', ?)")
        .bind::<String>(uuid::Uuid::new_v4().into())
        
        .execute(pool)
        .await
        .context("Failed to insert users")?;
    Ok(())
}


fn test_cedar() {
    const POLICY_SRC: &str = r#"
    permit (
        principal in Role::"supervisor",
        action == Action::"approveSchedule",
        resource
    ); "#;
    /*
                 when {
                  resource.type == "Schedule" &&
                  resource.epochSeconds > context.thirtyDaysAgoSeconds
                };
                "#;
            *//*    forbid (principal, action, resource) when {
            principal.behavior == "bad"
        };
    */
    let thirty_days_ago =
        (jiff::Timestamp::now() - jiff::SignedDuration::from_hours(30 * 24)).as_second();

    let context_json = serde_json::json!({
        "thirtyDaysAgoSeconds": thirty_days_ago,
    });

    let entities_json = serde_json::json!([
        {
            "uid": {
                "type": "Role",
                "id": "supervisor"
            },
            "attrs": {},
            "parents": []
        },
        {
            "uid": {
                "type": "User",
                "id": "fred"
            },
            "attrs": {},
            "parents": [
                {
                    "type": "Role",
                    "id": "supervisor"
                }
            ]
        },
        {
            "uid": {
                "type": "User",
                "id": "bob"
            },
            "attrs": {},
            "parents": []
        },
        {
            "uid": {
                "type": "User",
                "id": "badBob"
            },
            "attrs": {
                "behavior": "bad"
            },
            "parents": [
                {
                    "type": "Role",
                    "id": "supervisor"
                }
            ]
        },
        {
            "uid": {
                "type": "Schedule",
                "id": "scheduleInScope"
            },
            "attrs": {
                "epochSeconds": thirty_days_ago + 1100,
            },
            "parents": [],
        },
        {
            "uid": {
                "type": "Schedule",
                "id": "scheduleOutOfScope"
            },
            "attrs": {
                "epochSeconds": thirty_days_ago - 1100,
            },
            "parents": [],
        }
    ]);

    let fred_principal = cedar_policy::EntityUid::from_str(r#"User::"fred""#).unwrap();
    let bob_principal = cedar_policy::EntityUid::from_str(r#"User::"bob""#).unwrap();
    let bad_bob_principal = cedar_policy::EntityUid::from_str(r#"User::"badBob""#).unwrap();

    let action = cedar_policy::EntityUid::from_str(r#"Action::"approveSchedule""#).unwrap();

    let resource_in_scope = cedar_policy::EntityUid::from_str(r#"Schedule::"scheduleInScope""#).unwrap();
    let resource_out_of_scope = cedar_policy::EntityUid::from_str(r#"Schedule::"scheduleOutOfScope""#).unwrap();

    let context = cedar_policy::Context::from_json_value(context_json, None).unwrap();

    let policy_set = cedar_policy::PolicySet::from_str(POLICY_SRC).unwrap();
    let entities = cedar_policy::Entities::from_json_value(entities_json, None).unwrap();

    let authorizer = cedar_policy::Authorizer::new();

    let fred_request_in_scope =
        cedar_policy::Request::new(fred_principal, action, resource_in_scope, context, None).unwrap();

    let decision = authorizer.is_authorized(&fred_request_in_scope, &policy_set, &entities);

    println!("{:?}", decision.decision());
}
