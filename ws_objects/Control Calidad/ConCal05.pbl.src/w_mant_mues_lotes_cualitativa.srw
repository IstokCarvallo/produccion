$PBExportHeader$w_mant_mues_lotes_cualitativa.srw
$PBExportComments$Muestra lista de lotes con misma identificación pero con distinto número de lote, para la planilla cuantitativa.
forward
global type w_mant_mues_lotes_cualitativa from w_mant_mues_ctlcalinspectores
end type
end forward

global type w_mant_mues_lotes_cualitativa from w_mant_mues_ctlcalinspectores
end type
global w_mant_mues_lotes_cualitativa w_mant_mues_lotes_cualitativa

on w_mant_mues_lotes_cualitativa.create
call super::create
end on

on w_mant_mues_lotes_cualitativa.destroy
call super::destroy
end on

type st_encabe from w_mant_mues_ctlcalinspectores`st_encabe within w_mant_mues_lotes_cualitativa
end type

type pb_nuevo from w_mant_mues_ctlcalinspectores`pb_nuevo within w_mant_mues_lotes_cualitativa
end type

type pb_lectura from w_mant_mues_ctlcalinspectores`pb_lectura within w_mant_mues_lotes_cualitativa
end type

type pb_eliminar from w_mant_mues_ctlcalinspectores`pb_eliminar within w_mant_mues_lotes_cualitativa
end type

type pb_insertar from w_mant_mues_ctlcalinspectores`pb_insertar within w_mant_mues_lotes_cualitativa
end type

type pb_salir from w_mant_mues_ctlcalinspectores`pb_salir within w_mant_mues_lotes_cualitativa
end type

type pb_imprimir from w_mant_mues_ctlcalinspectores`pb_imprimir within w_mant_mues_lotes_cualitativa
end type

type pb_grabar from w_mant_mues_ctlcalinspectores`pb_grabar within w_mant_mues_lotes_cualitativa
end type

type dw_1 from w_mant_mues_ctlcalinspectores`dw_1 within w_mant_mues_lotes_cualitativa
end type

