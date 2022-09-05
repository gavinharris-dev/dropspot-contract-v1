#if defined(DEBUG)
#define TRACE_IF_FALSE(a,b,c) traceIfFalse a c
#define TRACE_ERROR(a, b) traceError a
#define TRACE(a, b, c) trace a c
#define FROM_BUILT_IN_DATA(m, c, a, t) case fromBuiltinData a :: Maybe t of { Nothing -> TRACE_ERROR(m, c); Just x -> x }
#define DataConstraint(a) FromData a
#else
#define TRACE_IF_FALSE(a,b,c) traceIfFalse b c
#define TRACE_ERROR(a, b) traceError b
#define TRACE(a, b, c) trace b c
#define FROM_BUILT_IN_DATA(m, c, a, t) unsafeFromBuiltinData a :: t
#define DataConstraint(a) UnsafeFromData a
#endif

#if defined(DEBUG) || defined(DEBUG_CLOSE)
#define TRACE_IF_FALSE_CLOSE(a, b) traceIfFalse a b
#else
#define TRACE_IF_FALSE_CLOSE(a, b) b
#endif