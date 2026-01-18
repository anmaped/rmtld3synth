async function getSchema() {
    try {
        if (window.backendMode) {
            const res = await fetch('api/schema');
            window.schema = await res.json();
            document.getElementById("configuration").classList.remove("d-none");
            console.log("Schema loaded:", window.schema);
            buildForm();
        } else {
            //const module = await import('./schema_local.js');
            //window.schema = module.schema;
        }

    } catch (error) {
        console.error('Error loading schema:', error);
    }
}


/* -----------------------------------------------------
   2. EXTRACT GROUP FROM DESCRIPTION
   Parses schema description for [group: Name] tag
   Returns "General" if no group tag found
------------------------------------------------------ */
function getGroupFromDescription(desc = "") {
    const m = desc.match(/\[group:\s*([^\]]+)\]/i);
    return m ? m[1].trim() : "General";
}


/* -----------------------------------------------------
   3. AUTO-GROUP PROPERTIES BY TAG
   Organizes schema properties into named groups
   based on [group:] tags in their descriptions
------------------------------------------------------ */
function groupProperties() {
    const groups = {};

    for (const [name, def] of Object.entries(schema.properties)) {
        const group = getGroupFromDescription(def.description);

        if (!groups[group]) groups[group] = [];
        groups[group].push({ name, def });
    }

    return groups;
}


/* -----------------------------------------------------
   4. FIELD GENERATION HELPER
   Creates HTML form fields based on JSON Schema types
   Supports: boolean, integer, string, oneOf, anyOf
   Event listeners are attached directly
------------------------------------------------------ */
function createField(name, def) {
    const wrapper = document.createElement("div");
    wrapper.className = "mb-3";

    const label = document.createElement("label");
    label.className = "form-label fw-semibold";
    label.textContent = name;

    const errorDiv = document.createElement("div");
    errorDiv.id = `error-${name}`;
    errorDiv.className = "error-text d-none text-danger mt-1";

    /* --- BOOLEAN --- */
    if (def.type === "boolean") {
        const checkboxWrapper = document.createElement("div");
        checkboxWrapper.className = "form-check";

        const input = document.createElement("input");
        input.type = "checkbox";
        input.className = "form-check-input";
        input.name = name;
        input.id = name;

        const lbl = document.createElement("label");
        lbl.className = "form-check-label";
        lbl.htmlFor = name;
        lbl.textContent = name;

        // set checked
        if (def.default === true) {
            input.checked = true;
        }

        checkboxWrapper.appendChild(input);
        checkboxWrapper.appendChild(lbl);
        wrapper.appendChild(checkboxWrapper);
        wrapper.appendChild(errorDiv);
        return wrapper;
    }

    /* --- INTEGER --- */
    if (def.type === "integer") {
        const input = document.createElement("input");
        input.type = "number";
        input.className = "form-control";
        input.name = name;
        input.id = name;
        input.step = "1"; // ensure integers only

        // set default value
        if (def.default !== undefined) {
            input.value = def.default;
        }

        wrapper.appendChild(label);
        wrapper.appendChild(input);
        wrapper.appendChild(errorDiv);
        return wrapper;
    }

    /* --- STRING --- */
    if (def.type === "string" && !def.enum) {
        const input = document.createElement("input");
        input.type = "text";
        input.className = "form-control";
        input.name = name;
        input.id = name;
        wrapper.appendChild(label);
        wrapper.appendChild(input);
        wrapper.appendChild(errorDiv);
        return wrapper;
    }

    /* --- ARRAY --- */
    if (def.type === "array" || (def.oneOf && def.oneOf.some(o => o.type === "array"))) {
        const textarea = document.createElement("textarea");
        textarea.className = "form-control";
        textarea.name = name;
        textarea.id = name;
        textarea.rows = 3;
        textarea.placeholder = "Enter expressions, one per line or comma-separated";
        wrapper.appendChild(label);
        wrapper.appendChild(textarea);
        wrapper.appendChild(errorDiv);
        return wrapper;
    }


    /* --- Enumeration --- */
    if (def.type === "string" && def.enum) {

        const stringOption = def;

        if (stringOption) {
            const select = document.createElement("select");
            select.className = "form-select";
            select.name = `${name}`;
            select.id = `${name}-select`;

            // Add enum options
            stringOption.enum.forEach(val => {
                const opt = document.createElement("option");
                opt.value = val;
                opt.textContent = val;
                select.appendChild(opt);
            });

            wrapper.appendChild(label);
            wrapper.appendChild(select);
            wrapper.appendChild(errorDiv);
            return wrapper;
        }

        console.warn(`Unsupported enum types for "${name}":`, types);
    }


    /* --- ONEOF --- */
    if (def.oneOf) {
        const types = def.oneOf.map(o => o.type);

        // Handle string + integer combination
        if (types.includes("string") && types.includes("integer")) {
            const stringOption = def.oneOf.find(o => o.type === "string" && o.enum);

            if (stringOption) {
                const select = document.createElement("select");
                select.className = "form-select";
                select.name = `${name}`;
                select.id = `${name}-select`;

                // Add enum options
                stringOption.enum.forEach(val => {
                    const opt = document.createElement("option");
                    opt.value = val;
                    opt.textContent = val;
                    select.appendChild(opt);
                });

                // Add "Other" option for number input
                const otherOption = document.createElement("option");
                otherOption.value = "__number__";
                otherOption.textContent = "Other (number)";
                select.appendChild(otherOption);

                // Create number input
                const numberInput = document.createElement("input");
                numberInput.type = "number";
                numberInput.className = "form-control mt-2";
                numberInput.name = name;
                numberInput.id = name;
                numberInput.style.display = "none";
                numberInput.step = "1"; // ensure integers only

                // Toggle number input visibility
                select.addEventListener("change", () => {
                    const isNumber = select.value === "__number__";
                    numberInput.style.display = isNumber ? "block" : "none";
                    numberInput.name = isNumber ? name : "";
                    select.name = isNumber ? "" : name;
                });

                wrapper.appendChild(label);
                wrapper.appendChild(select);
                wrapper.appendChild(numberInput);
                wrapper.appendChild(errorDiv);
                return wrapper;
            }
        }

        // Handle array + string combination
        if (types.includes("array") && types.includes("string")) {
            const input = document.createElement("input");
            input.type = "text";
            input.className = "form-control";
            input.name = name;
            input.id = name;
            input.placeholder = "Single value or comma-separated list";
            wrapper.appendChild(label);
            wrapper.appendChild(input);
            wrapper.appendChild(errorDiv);
            return wrapper;
        }

        console.warn(`Unsupported oneOf types for "${name}":`, types);
    }

    console.warn(`Unsupported field type for "${name}"`);
    return wrapper;
}




/* -----------------------------------------------------
   5. BUILD BOOTSTRAP FORM BASED ON GROUPS
   Dynamically generates Bootstrap cards for each group
   containing the appropriate form fields
------------------------------------------------------ */
function buildForm() {
    const form = document.getElementById("dynamic-form");
    form.innerHTML = ""; // Clear previous form

    const groups = groupProperties();

    for (const [groupName, fields] of Object.entries(groups)) {
        const card = document.createElement("div");
        card.className = "card mb-3";

        const header = document.createElement("div");
        header.className = "card-header fw-bold";
        header.textContent = groupName;

        const body = document.createElement("div");
        body.className = "card-body";

        // Append each field element directly
        fields.forEach(({ name, def }) => {
            const fieldElement = createField(name, def);
            body.appendChild(fieldElement);
        });

        // Add validation error message for Input group (anyOf requirement)
        if (groupName.toLowerCase().includes("input")) {
            const errorDiv = document.createElement("div");
            errorDiv.id = "anyof-error";
            errorDiv.className = "error-text d-none mt-2";
            errorDiv.textContent = "At least one of these inputs is required.";
            body.appendChild(errorDiv);
        }

        card.appendChild(header);
        card.appendChild(body);
        form.appendChild(card);
    }
}



/* -----------------------------------------------------
   6. SUBMIT HANDLER → GENERATE JSON
   Collects form data, processes special field types,
   and displays the resulting JSON configuration
------------------------------------------------------ */
function updateOutput() {

    const form = document.getElementById("dynamic-form");

    const data = {};
    const formData = new FormData(form);

    // Collect all non-empty form field values
    formData.forEach((value, key) => {
        if (value !== "") data[key] = value;
    });

    // Handle unchecked checkboxes
    form.querySelectorAll("input[type=checkbox]").forEach(cb => {
        if (cb.checked) data[cb.name] = cb.checked;
    });

    // Convert integer fields to numbers
    Object.keys(data).forEach(key => {
        const def = window.schema.properties[key];
        if (def && def.type === "integer") {
            data[key] = Number(data[key]);
        }
    });

    // Convert number fields from oneOf selections
    Object.keys(data).forEach(key => {
        const def = window.schema.properties[key];

        // Integer fields
        if (def && def.type === "integer") {
            if (/^-?\d+$/.test(data[key])) {
                data[key] = parseInt(data[key], 10);
            } else {
                console.warn(`Invalid integer for field ${key}:`, data[key]);
                delete data[key]; // optional: skip invalid input
            }
        }

        // oneOf number conversion
        if (def && def.oneOf) {
            const types = def.oneOf.map(o => o.type);
            if (types.includes("integer")) {
                if (/^-?\d+$/.test(data[key])) {
                    data[key] = parseInt(data[key], 10);
                }
            }
        }
    });

    // Convert array inputs (comma-separated or newline-separated)
    Object.keys(data).forEach(key => {
        const def = window.schema.properties[key];
        if (def && (def.type === "array" || (def.oneOf && def.oneOf.some(o => o.type === "array")))) {
            if (typeof data[key] === "string" && data[key].trim()) {
                // Split by newlines first, then by commas not inside brackets
                const lines = data[key].split(/\n/).map(v => v.trim()).filter(v => v !== "");
                const items = [];
                lines.forEach(line => {
                    // Split by commas, but keep commas inside brackets together
                    const parts = line.split(/,(?![^\[]*\])/);
                    items.push(...parts.map(v => v.trim()).filter(v => v !== ""));
                });
                data[key] = items;
            }
        }
    });

    // Validate data against schema
    const valid = validateData(data);

    if (valid) {
        inputEditor.setValue(JSON.stringify(data, null, 2));
    } else {
        inputEditor.setValue("Validation errors. Please fix the highlighted fields. Current data:\n\n" + JSON.stringify(data, null, 2));
    }
}

/* -----------------------------------------------------
   7. VALIDATION USING AJV
   Validates the generated JSON against the schema
   Displays error messages next to invalid fields
------------------------------------------------------ */
function validateData(data) {
    const ajv = new window.ajv7({ allErrors: true, strict: false });

    //const ajv = new Ajv({ allErrors: true, strict: false });
    const validate = ajv.compile(window.schema); // schema loaded in getSchema()
    const valid = validate(data);

    // Clear previous errors
    document.querySelectorAll(".error-text").forEach(el => {
        el.textContent = "";
        el.classList.add("d-none");
    });

    if (!valid) {
        validate.errors.forEach(err => {
            console.log(err); // for debugging
            // Map Ajv error path to your input name
            let fieldName = err.instancePath.replace(/^\//, '').replace(/\//g, '-');

            // Special handling for oneOf / array inputs
            if (!fieldName && err.params && err.params.missingProperty) {
                fieldName = err.params.missingProperty;
            }

            const errorDiv = document.getElementById(`error-${fieldName}`);
            if (errorDiv) {
                errorDiv.textContent = err.message;
                errorDiv.classList.remove("d-none");
            }
        });
    }

    return valid;
}

document.getElementById("submit-btn").addEventListener("click", () => {
    updateOutput();
});

document.getElementById("dynamic-form").addEventListener("blur", (e) => {

    const disableBlur = document.getElementById("disable-blur-check").checked;
    if (disableBlur) {
        log("Auto-update disabled.", "text-info");
        return;
    }

    log("--------------------------------------------------", "text-info");
    log("Field blurred: " + e.target.name, "text-info");
    log("user clicks away, tabs away, or focus moves elsewhere", "text-info");
    log("--------------------------------------------------", "text-info");
    log("Submit current JSON:", "text-info");
    updateOutput();
}, true);

document.getElementById("reset-btn").addEventListener("click", () => {
    log("Resetting form to default values.", "text-warning");
    buildForm();
});

document.getElementById("auto-fill-btn").addEventListener("click", () => {
    log("Filling form with input configuration.", "text-info");
    // not implemented yet
    log("Auto-fill not implemented yet.", "text-warning");
});