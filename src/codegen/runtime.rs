//! Runtime function declarations and helpers

use inkwell::AddressSpace;
use inkwell::types::BasicMetadataTypeEnum;
use inkwell::values::{FunctionValue, IntValue, PointerValue};

use crate::codegen::Codegen;

/// Runtime function names
pub mod names {
    // Memory allocation
    pub const GC_MALLOC: &str = "GC_malloc";
    pub const GC_REALLOC: &str = "GC_realloc";
    pub const GC_INIT: &str = "GC_init";

    // String operations
    pub const GO_STRING_CONCAT: &str = "go_string_concat";
    pub const GO_STRING_COMPARE: &str = "go_string_compare";
    pub const GO_STRING_FROM_BYTES: &str = "go_string_from_bytes";
    pub const GO_STRING_TO_BYTES: &str = "go_string_to_bytes";
    pub const GO_STRING_FROM_CSTR: &str = "go_string_from_cstr";
    pub const GO_STRING_DECODE_RUNE: &str = "go_string_decode_rune";

    // Slice operations
    pub const GO_SLICE_APPEND: &str = "go_slice_append";
    pub const GO_SLICE_COPY: &str = "go_slice_copy";
    pub const GO_SLICE_GROW: &str = "go_slice_grow";

    // Map operations
    pub const GO_MAP_MAKE: &str = "go_map_make";
    pub const GO_MAP_GET: &str = "go_map_get";
    pub const GO_MAP_SET: &str = "go_map_set";
    pub const GO_MAP_DELETE: &str = "go_map_delete";
    pub const GO_MAP_LEN: &str = "go_map_len";
    pub const GO_MAP_ITER_INIT: &str = "go_map_iter_init";
    pub const GO_MAP_ITER_NEXT: &str = "go_map_iter_next";
    pub const GO_MAP_ITER_KEY: &str = "go_map_iter_key";
    pub const GO_MAP_ITER_VALUE: &str = "go_map_iter_value";

    // Print operations
    pub const GO_PRINT_INT: &str = "go_print_int";
    pub const GO_PRINT_UINT: &str = "go_print_uint";
    pub const GO_PRINT_FLOAT: &str = "go_print_float";
    pub const GO_PRINT_BOOL: &str = "go_print_bool";
    pub const GO_PRINT_STRING: &str = "go_print_string";
    pub const GO_PRINT_POINTER: &str = "go_print_pointer";
    pub const GO_PRINT_SPACE: &str = "go_print_space";
    pub const GO_PRINT_NEWLINE: &str = "go_print_newline";

    // Panic
    pub const GO_PANIC: &str = "go_panic";
    pub const GO_PANIC_BOUNDS: &str = "go_panic_bounds";
    pub const GO_PANIC_NIL: &str = "go_panic_nil";

    // C standard library
    pub const MEMCPY: &str = "memcpy";
    pub const MEMCMP: &str = "memcmp";
}

impl<'ctx> Codegen<'ctx> {
    pub fn declare_runtime_functions(&self) {
        let ptr = self.context.ptr_type(AddressSpace::default());
        let i64_ty = self.context.i64_type();
        let i32_ty = self.context.i32_type();
        let i8_ty = self.context.i8_type();
        let f64_ty = self.context.f64_type();
        let void_ty = self.context.void_type();
        let string_ty = self.string_type();
        let slice_ty = self.slice_type();

        // GC_malloc(size: i64) -> ptr
        let gc_malloc_ty = ptr.fn_type(&[i64_ty.into()], false);
        self.module
            .add_function(names::GC_MALLOC, gc_malloc_ty, None);

        // GC_realloc(ptr: ptr, size: i64) -> ptr
        let gc_realloc_ty = ptr.fn_type(&[ptr.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GC_REALLOC, gc_realloc_ty, None);

        // GC_init() -> void
        let gc_init_ty = void_ty.fn_type(&[], false);
        self.module.add_function(names::GC_INIT, gc_init_ty, None);

        // go_string_concat(a: string, b: string) -> string
        let str_meta: BasicMetadataTypeEnum = string_ty.into();
        let concat_ty = string_ty.fn_type(&[str_meta, str_meta], false);
        self.module
            .add_function(names::GO_STRING_CONCAT, concat_ty, None);

        // go_string_compare(a: string, b: string) -> i32
        let cmp_ty = i32_ty.fn_type(&[str_meta, str_meta], false);
        self.module
            .add_function(names::GO_STRING_COMPARE, cmp_ty, None);

        // go_string_from_cstr(cstr: ptr) -> string
        let from_cstr_ty = string_ty.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_STRING_FROM_CSTR, from_cstr_ty, None);

        // go_string_from_bytes(data: ptr, len: i64) -> string
        let from_bytes_ty = string_ty.fn_type(&[ptr.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GO_STRING_FROM_BYTES, from_bytes_ty, None);

        // go_string_to_bytes(s: string) -> slice
        let slice_meta: BasicMetadataTypeEnum = slice_ty.into();
        let to_bytes_ty = slice_ty.fn_type(&[str_meta], false);
        self.module
            .add_function(names::GO_STRING_TO_BYTES, to_bytes_ty, None);

        // go_string_decode_rune(data: ptr, len: i64) -> i64
        // Returns: rune in lower 32 bits, byte width in upper 32 bits
        let decode_rune_ty = i64_ty.fn_type(&[ptr.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GO_STRING_DECODE_RUNE, decode_rune_ty, None);

        // go_slice_append(result: *slice, slice: *slice, elem_ptr: ptr, elem_size: i64)
        let append_ty =
            void_ty.fn_type(&[ptr.into(), ptr.into(), ptr.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GO_SLICE_APPEND, append_ty, None);

        // go_slice_copy(dst: *slice, src: *slice, elem_size: i64) -> i64
        let copy_ty = i64_ty.fn_type(&[ptr.into(), ptr.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GO_SLICE_COPY, copy_ty, None);

        // go_slice_grow(result: *slice, slice: *slice, newcap: i64, elem_size: i64)
        let grow_ty = void_ty.fn_type(
            &[ptr.into(), ptr.into(), i64_ty.into(), i64_ty.into()],
            false,
        );
        self.module
            .add_function(names::GO_SLICE_GROW, grow_ty, None);

        // go_map_make(key_size: i64, value_size: i64, key_type: i64, hint: i64) -> ptr
        let map_make_ty = ptr.fn_type(
            &[i64_ty.into(), i64_ty.into(), i64_ty.into(), i64_ty.into()],
            false,
        );
        self.module
            .add_function(names::GO_MAP_MAKE, map_make_ty, None);

        // go_map_get(m: ptr, key: ptr, key_size: i64, value_size: i64) -> ptr
        let map_get_ty = ptr.fn_type(
            &[ptr.into(), ptr.into(), i64_ty.into(), i64_ty.into()],
            false,
        );
        self.module
            .add_function(names::GO_MAP_GET, map_get_ty, None);

        // go_map_set(m: ptr, key: ptr, value: ptr, key_size: i64, value_size: i64)
        let map_set_ty = void_ty.fn_type(
            &[
                ptr.into(),
                ptr.into(),
                ptr.into(),
                i64_ty.into(),
                i64_ty.into(),
            ],
            false,
        );
        self.module
            .add_function(names::GO_MAP_SET, map_set_ty, None);

        // go_map_delete(m: ptr, key: ptr, key_size: i64)
        let map_delete_ty = void_ty.fn_type(&[ptr.into(), ptr.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GO_MAP_DELETE, map_delete_ty, None);

        // go_map_len(m: ptr) -> i64
        let map_len_ty = i64_ty.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_MAP_LEN, map_len_ty, None);

        // go_map_iter_init(m: ptr) -> ptr
        let iter_init_ty = ptr.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_MAP_ITER_INIT, iter_init_ty, None);

        // go_map_iter_next(iter: ptr) -> i32
        let iter_next_ty = i32_ty.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_MAP_ITER_NEXT, iter_next_ty, None);

        // go_map_iter_key(iter: ptr) -> ptr
        let iter_key_ty = ptr.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_MAP_ITER_KEY, iter_key_ty, None);

        // go_map_iter_value(iter: ptr) -> ptr
        let iter_value_ty = ptr.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_MAP_ITER_VALUE, iter_value_ty, None);

        // Print functions
        // go_print_int(v: i64)
        let print_int_ty = void_ty.fn_type(&[i64_ty.into()], false);
        self.module
            .add_function(names::GO_PRINT_INT, print_int_ty, None);

        // go_print_uint(v: u64)
        let print_uint_ty = void_ty.fn_type(&[i64_ty.into()], false);
        self.module
            .add_function(names::GO_PRINT_UINT, print_uint_ty, None);

        // go_print_float(v: f64)
        let print_float_ty = void_ty.fn_type(&[f64_ty.into()], false);
        self.module
            .add_function(names::GO_PRINT_FLOAT, print_float_ty, None);

        // go_print_bool(v: i8)
        let print_bool_ty = void_ty.fn_type(&[i8_ty.into()], false);
        self.module
            .add_function(names::GO_PRINT_BOOL, print_bool_ty, None);

        // go_print_string(s: string)
        let print_string_ty = void_ty.fn_type(&[str_meta], false);
        self.module
            .add_function(names::GO_PRINT_STRING, print_string_ty, None);

        // go_print_pointer(p: ptr)
        let print_ptr_ty = void_ty.fn_type(&[ptr.into()], false);
        self.module
            .add_function(names::GO_PRINT_POINTER, print_ptr_ty, None);

        // go_print_space()
        let print_space_ty = void_ty.fn_type(&[], false);
        self.module
            .add_function(names::GO_PRINT_SPACE, print_space_ty, None);

        // go_print_newline()
        let print_newline_ty = void_ty.fn_type(&[], false);
        self.module
            .add_function(names::GO_PRINT_NEWLINE, print_newline_ty, None);

        // Panic functions
        // go_panic(msg: string)
        let panic_ty = void_ty.fn_type(&[str_meta], false);
        self.module.add_function(names::GO_PANIC, panic_ty, None);

        // go_panic_bounds(index: i64, len: i64)
        let panic_bounds_ty = void_ty.fn_type(&[i64_ty.into(), i64_ty.into()], false);
        self.module
            .add_function(names::GO_PANIC_BOUNDS, panic_bounds_ty, None);

        // go_panic_nil()
        let panic_nil_ty = void_ty.fn_type(&[], false);
        self.module
            .add_function(names::GO_PANIC_NIL, panic_nil_ty, None);

        // memcmp(a: ptr, b: ptr, n: i64) -> i32
        let memcmp_ty = i32_ty.fn_type(&[ptr.into(), ptr.into(), i64_ty.into()], false);
        self.module.add_function(names::MEMCMP, memcmp_ty, None);

        // memcpy(dst: ptr, src: ptr, n: i64) -> ptr
        let memcpy_ty = ptr.fn_type(&[ptr.into(), ptr.into(), i64_ty.into()], false);
        self.module.add_function(names::MEMCPY, memcpy_ty, None);

        // Silence unused warning
        let _ = slice_meta;
    }

    pub fn get_runtime_fn(&self, name: &str) -> FunctionValue<'ctx> {
        self.module
            .get_function(name)
            .unwrap_or_else(|| panic!("codegen bug: runtime function '{}' not declared", name))
    }

    pub fn call_gc_malloc(&self, size: IntValue<'ctx>) -> PointerValue<'ctx> {
        let func = self.get_runtime_fn(names::GC_MALLOC);
        let call = self
            .builder
            .build_call(func, &[size.into()], "malloc")
            .unwrap();
        call.try_as_basic_value()
            .basic()
            .expect("codegen bug: GC_malloc should return a pointer")
            .into_pointer_value()
    }
}
