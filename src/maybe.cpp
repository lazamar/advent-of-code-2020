template<typename T>
class Maybe {
    public:
        Maybe() {
            hasValue = false;
        }

        Maybe(T t) {
            hasValue = true;
            value = t;
        }

        bool isJust() {
            return hasValue;
        }

        T getValue() {
            return value;
        }

        T withDefault(T def) {
            if (hasValue) return value;
            return def;
        }

        template<typename R>
        R run(R has(T), R hasnt(void)) {
            if (hasValue) return has(value);
            return hasnt();
        }

    private:
        T value; // may not be set
        bool hasValue;
};

template<typename T>
Maybe<T>
Just(T t)
{
    return Maybe<T>(t);
}

template<typename T>
Maybe<T>
Nothing()
{
    return Maybe<T>();
}

