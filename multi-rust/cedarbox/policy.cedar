forbid (principal, action, resource)
unless { 
    resource.organizationId == principal.organizationId ||
    principal.role.crossOrganizationAccess
};

permit (
    principal in ?principal,
    action == Action::"createOrganization",
    resource
) when {
    principal.role.crossOrganizationAccess
};

permit (
    principal in ?principal,
    action in [
        Action::"createSchedule",
        Action::"createUser",
        Action::"createRole",
        Action::"editOrganization"
    ],
    resource is Organization
) when {
    principal.role.allowedActions.contains(action)
};

permit (
    principal in ?principal,
    action in [
        Action::"createJob",
        Action::"editSchedule"
    ],
    resource is Schedule
) when {
    principal.role.allowedActions.contains(action)
};

permit (
    principal in ?principal,
    action in [
        Action::"editJob"
    ],
    resource is Job
) when {
    principal.role.allowedActions.contains(action)
};

permit (
    principal in ?principal,
    action in [
        Action::"assignRole",
        Action::"editUser"
    ],
    resource is User
) when {
    principal.role.allowedActions.contains(action)
};

permit (
    principal in ?principal,
    action in [
        Action::"editRole"
    ],
    resource is Role
) when {
    principal.role.allowedActions.contains(action)
};