CLASS zcl_ztest_user_odat_01_dpc_ext DEFINITION
  PUBLIC
  INHERITING FROM zcl_ztest_user_odat_01_dpc
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS /iwbep/if_mgw_appl_srv_runtime~create_deep_entity
        REDEFINITION .
    METHODS /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity
        REDEFINITION .
    METHODS /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset
        REDEFINITION .
  PROTECTED SECTION.

    METHODS userset_create_entity
        REDEFINITION .
    METHODS userset_get_entity
        REDEFINITION .
    METHODS userset_get_entityset
        REDEFINITION .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_ZTEST_USER_ODAT_01_DPC_EXT IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZTEST_USER_ODAT_01_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~CREATE_DEEP_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IO_DATA_PROVIDER               TYPE REF TO /IWBEP/IF_MGW_ENTRY_PROVIDER
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IO_EXPAND                      TYPE REF TO /IWBEP/IF_MGW_ODATA_EXPAND
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_C(optional)
* | [<---] ER_DEEP_ENTITY                 TYPE REF TO DATA
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~create_deep_entity.

    TYPES: BEGIN OF ty_contactset,
             phone TYPE ztest_contacts-phone,
             type  TYPE ztest_contacts-type,
           END OF ty_contactset.

    TYPES: BEGIN OF ty_userset_deep,
             uname       TYPE ztest_user-uname,
             name_first  TYPE ztest_user-name_first,
             name_last   TYPE ztest_user-name_last,
             zz_nickname TYPE ztest_user-zz_nickname,
             smtp_addr   TYPE ztest_user-smtp_addr,
             contactset  TYPE STANDARD TABLE OF ty_contactset WITH DEFAULT KEY,
           END OF ty_userset_deep.

    DATA: ls_userset_deep   TYPE ty_userset_deep,
          ls_ztest_user     TYPE ztest_user,
          lt_ztest_contacts TYPE STANDARD TABLE OF ztest_contacts,
          ls_ztest_contacts TYPE ztest_contacts.

    DATA: lv_error TYPE abap_bool.

    CLEAR er_deep_entity.

    CASE iv_entity_set_name.

      WHEN 'UserSet'.

        io_data_provider->read_entry_data(
          IMPORTING
            es_data = ls_userset_deep ).

        ls_ztest_user = CORRESPONDING #( ls_userset_deep ).
        INSERT ztest_user FROM @ls_ztest_user.

        IF sy-subrc EQ 0.

          lt_ztest_contacts = CORRESPONDING #( ls_userset_deep-contactset ).
          ls_ztest_contacts-uname = ls_userset_deep-uname.
          MODIFY lt_ztest_contacts FROM ls_ztest_contacts TRANSPORTING uname
            WHERE table_line IS NOT INITIAL.

          INSERT ztest_contacts FROM TABLE @lt_ztest_contacts.
          IF sy-subrc NE 0.
            lv_error = abap_true.
          ENDIF.

        ELSE.

          lv_error = abap_true.

        ENDIF.

        IF lv_error EQ abap_false.

          COMMIT WORK.

          copy_data_to_ref(
            EXPORTING
              is_data = ls_userset_deep
            CHANGING
              cr_data = er_deep_entity ).

        ELSE.

          ROLLBACK WORK.

          RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
            EXPORTING
              textid  = /iwbep/cx_mgw_busi_exception=>business_error
              message = 'Error during creation of user' ##NO_TEXT.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZTEST_USER_ODAT_01_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IO_EXPAND                      TYPE REF TO /IWBEP/IF_MGW_ODATA_EXPAND(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [<---] ER_ENTITY                      TYPE REF TO DATA
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [<---] ET_EXPANDED_CLAUSES            TYPE        STRING_TABLE
* | [<---] ET_EXPANDED_TECH_CLAUSES       TYPE        STRING_TABLE
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entity.

    DATA: BEGIN OF ls_user.
        INCLUDE TYPE zcl_ztest_user_odat_02_mpc=>ts_user.
    DATA contactset TYPE zcl_ztest_user_odat_02_mpc=>tt_contact.
    DATA: END OF ls_user.

    REFRESH: et_expanded_tech_clauses,
             et_expanded_clauses.

    CLEAR: er_entity,
           es_response_context.

    TRY.
        DATA(lv_uname) = CONV ztest_user-uname( it_key_tab[ name = 'UserName' ]-value ).
      CATCH cx_sy_itab_line_not_found.
        CLEAR lv_uname.
    ENDTRY.

    CASE iv_entity_set_name.

      WHEN 'UserSet'.

        DATA(lt_expand) = io_expand->get_children( ).

        IF line_exists( lt_expand[ tech_nav_prop_name = 'CONTACTSET' ] ).

          SELECT SINGLE *
          INTO CORRESPONDING FIELDS OF @ls_user
          FROM ztest_user
          WHERE uname EQ @lv_uname.

          IF sy-subrc EQ 0.

            SELECT *
            INTO TABLE @ls_user-contactset
            FROM ztest_contacts
            WHERE uname EQ @lv_uname.

            et_expanded_tech_clauses = VALUE #( ( |ContactSet |  ) ).

            copy_data_to_ref(
              EXPORTING
                is_data = ls_user
              CHANGING
                cr_data = er_entity ).

          ENDIF.

        ENDIF.

    ENDCASE.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method ZCL_ZTEST_USER_ODAT_01_DPC_EXT->/IWBEP/IF_MGW_APPL_SRV_RUNTIME~GET_EXPANDED_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING(optional)
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING(optional)
* | [--->] IV_SOURCE_NAME                 TYPE        STRING(optional)
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION(optional)
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER(optional)
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH(optional)
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR(optional)
* | [--->] IV_FILTER_STRING               TYPE        STRING(optional)
* | [--->] IV_SEARCH_STRING               TYPE        STRING(optional)
* | [--->] IO_EXPAND                      TYPE REF TO /IWBEP/IF_MGW_ODATA_EXPAND(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ER_ENTITYSET                   TYPE REF TO DATA
* | [<---] ET_EXPANDED_CLAUSES            TYPE        STRING_TABLE
* | [<---] ET_EXPANDED_TECH_CLAUSES       TYPE        STRING_TABLE
* | [<---] ES_RESPONSE_CONTEXT            TYPE        TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD /iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset.
    TRY.
        super->/iwbep/if_mgw_appl_srv_runtime~get_expanded_entityset(
          EXPORTING
            iv_entity_name           = iv_entity_name
            iv_entity_set_name       = iv_entity_set_name
            iv_source_name           = iv_source_name
            it_filter_select_options = it_filter_select_options
            it_order                 = it_order
            is_paging                = is_paging
            it_navigation_path       = it_navigation_path
            it_key_tab               = it_key_tab
            iv_filter_string         = iv_filter_string
            iv_search_string         = iv_search_string
            io_expand                = io_expand
            io_tech_request_context  = io_tech_request_context
          IMPORTING
            er_entityset             = er_entityset
            et_expanded_clauses      = et_expanded_clauses
            et_expanded_tech_clauses = et_expanded_tech_clauses
            es_response_context      = es_response_context ).
      CATCH /iwbep/cx_mgw_busi_exception ##NO_HANDLER.
      CATCH /iwbep/cx_mgw_tech_exception ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZTEST_USER_ODAT_01_DPC_EXT->USERSET_CREATE_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY_C(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IO_DATA_PROVIDER               TYPE REF TO /IWBEP/IF_MGW_ENTRY_PROVIDER(optional)
* | [<---] ER_ENTITY                      TYPE        ZCL_ZTEST_USER_ODAT_02_MPC=>TS_USER
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD userset_create_entity.

    DATA: ls_userset TYPE ztest_user.

    CLEAR er_entity.

    io_data_provider->read_entry_data(
      IMPORTING
        es_data = ls_userset ).

    INSERT ztest_user FROM @ls_userset.

    IF sy-subrc EQ 0.

      COMMIT WORK.
      er_entity = ls_userset.

    ELSE.

      RAISE EXCEPTION TYPE /iwbep/cx_mgw_busi_exception
        EXPORTING
          textid  = /iwbep/cx_mgw_busi_exception=>business_error
          message = 'Error during creation of user' ##NO_TEXT.

    ENDIF.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZTEST_USER_ODAT_01_DPC_EXT->USERSET_GET_ENTITY
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IO_REQUEST_OBJECT              TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITY(optional)
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [<---] ER_ENTITY                      TYPE        ZCL_ZTEST_USER_ODAT_02_MPC=>TS_USER
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_ENTITY_CNTXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD userset_get_entity.

    CLEAR es_response_context.

    TRY.
        DATA(lv_uname) = CONV ztest_user-uname( it_key_tab[ name = 'UserName' ]-value ).
      CATCH cx_sy_itab_line_not_found.
        CLEAR lv_uname.
    ENDTRY.

    SELECT SINGLE *
    INTO @er_entity
    FROM ztest_user
    WHERE uname EQ @lv_uname.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method ZCL_ZTEST_USER_ODAT_01_DPC_EXT->USERSET_GET_ENTITYSET
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ENTITY_NAME                 TYPE        STRING
* | [--->] IV_ENTITY_SET_NAME             TYPE        STRING
* | [--->] IV_SOURCE_NAME                 TYPE        STRING
* | [--->] IT_FILTER_SELECT_OPTIONS       TYPE        /IWBEP/T_MGW_SELECT_OPTION
* | [--->] IS_PAGING                      TYPE        /IWBEP/S_MGW_PAGING
* | [--->] IT_KEY_TAB                     TYPE        /IWBEP/T_MGW_NAME_VALUE_PAIR
* | [--->] IT_NAVIGATION_PATH             TYPE        /IWBEP/T_MGW_NAVIGATION_PATH
* | [--->] IT_ORDER                       TYPE        /IWBEP/T_MGW_SORTING_ORDER
* | [--->] IV_FILTER_STRING               TYPE        STRING
* | [--->] IV_SEARCH_STRING               TYPE        STRING
* | [--->] IO_TECH_REQUEST_CONTEXT        TYPE REF TO /IWBEP/IF_MGW_REQ_ENTITYSET(optional)
* | [<---] ET_ENTITYSET                   TYPE        ZCL_ZTEST_USER_ODAT_02_MPC=>TT_USER
* | [<---] ES_RESPONSE_CONTEXT            TYPE        /IWBEP/IF_MGW_APPL_SRV_RUNTIME=>TY_S_MGW_RESPONSE_CONTEXT
* | [!CX!] /IWBEP/CX_MGW_BUSI_EXCEPTION
* | [!CX!] /IWBEP/CX_MGW_TECH_EXCEPTION
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD userset_get_entityset.

    CONSTANTS: lc_property_uname TYPE string VALUE 'UserName' ##NO_TEXT,
               lc_property_nname TYPE string VALUE 'Nickname' ##NO_TEXT.

    DATA lv_order_syntax TYPE string.

    REFRESH et_entityset.
    CLEAR es_response_context.

    TRY.
        DATA(ls_selopt_uname) = it_filter_select_options[ property = lc_property_uname ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR ls_selopt_uname.
    ENDTRY.

    TRY.
        DATA(ls_selopt_zz_nickname) = it_filter_select_options[ property = lc_property_nname ].
      CATCH cx_sy_itab_line_not_found.
        CLEAR ls_selopt_zz_nickname.
    ENDTRY.

    IF io_tech_request_context->has_count( ).

      SELECT COUNT( * )
      INTO @DATA(lv_count)
      FROM ztest_user
      WHERE uname IN @ls_selopt_uname-select_options AND
            zz_nickname IN @ls_selopt_zz_nickname-select_options.

      IF sy-subrc EQ 0.
        es_response_context-count = lv_count.
      ENDIF.

    ELSE.

*      DATA(lt_order) = VALUE abap_sortorder_tab( FOR <fs_order> IN it_order (
*        name = SWITCH #( <fs_order>-property
*                  WHEN 'Nickname' THEN 'ZZ_NICKNAME' )
*        descending = SWITCH #( <fs_order>-order
*                  WHEN 'desc' THEN abap_true )
*      ) ).

      LOOP AT it_order ASSIGNING FIELD-SYMBOL(<fs_order>).

        lv_order_syntax = lv_order_syntax &&
            SWITCH string( <fs_order>-property
                WHEN 'Nickname' THEN |, ZZ_NICKNAME| ) &&
            SWITCH string( <fs_order>-order
                WHEN 'desc' THEN | DESCENDING| ).

      ENDLOOP.

      SHIFT lv_order_syntax BY 2 PLACES LEFT.
*      DELETE lt_order WHERE name IS INITIAL.

      DATA(lv_top) = CONV i( io_tech_request_context->get_top( ) ).
      DATA(lv_skip) = CONV i( io_tech_request_context->get_skip( ) ).
      DATA(lv_rows) = lv_top + lv_skip.

*      SELECT *
*      INTO TABLE @DATA(lt_entityset)
*      FROM ztest_user
*      UP TO @lv_rows ROWS
*      WHERE uname IN @ls_selopt_uname-select_options AND
*            zz_nickname IN @ls_selopt_zz_nickname-select_options.
      SELECT *
      INTO TABLE @DATA(lt_entityset)
      FROM ztest_user
      UP TO @lv_rows ROWS
      WHERE uname IN @ls_selopt_uname-select_options AND
            zz_nickname IN @ls_selopt_zz_nickname-select_options
      ORDER BY (lv_order_syntax).

      IF sy-subrc EQ 0.

*        SORT lt_entityset BY (lt_order).

        IF lv_top IS NOT INITIAL OR lv_skip IS NOT INITIAL.

          DATA(lv_start) = lv_skip + 1.
          DATA(lv_end) = lv_skip + lv_top.
          lv_rows = lines( lt_entityset ).

          IF lv_start LE lv_rows.

            IF lv_end GT lv_rows.
              lv_end = lv_rows.
            ENDIF.

            APPEND LINES OF lt_entityset FROM lv_start TO lv_end TO et_entityset.

          ENDIF.

        ELSE.

          et_entityset = lt_entityset.

        ENDIF.

      ENDIF.

      IF io_tech_request_context->has_inlinecount( ) EQ abap_true.
        es_response_context-inlinecount = lines( et_entityset ).
      ELSE.
        CLEAR es_response_context-inlinecount.
      ENDIF.

    ENDIF.

  ENDMETHOD.
ENDCLASS.