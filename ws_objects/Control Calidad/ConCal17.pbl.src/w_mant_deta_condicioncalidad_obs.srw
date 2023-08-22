$PBExportHeader$w_mant_deta_condicioncalidad_obs.srw
forward
global type w_mant_deta_condicioncalidad_obs from w_mant_detalle_csd
end type
end forward

global type w_mant_deta_condicioncalidad_obs from w_mant_detalle_csd
integer width = 2409
integer height = 1244
string title = "FORMA DE PAGO"
end type
global w_mant_deta_condicioncalidad_obs w_mant_deta_condicioncalidad_obs

type variables
	
end variables

on w_mant_deta_condicioncalidad_obs.create
call super::create
end on

on w_mant_deta_condicioncalidad_obs.destroy
call super::destroy
end on

event ue_recuperadatos;call super::ue_recuperadatos;ias_campo[1]  = dw_1.Object.coca_observ[il_fila]

end event

event ue_deshace;call super::ue_deshace;If UpperBound(ias_campo) > 0 Then dw_1.SetItem(il_fila, "coca_observ", ias_campo[1])


end event

type pb_ultimo from w_mant_detalle_csd`pb_ultimo within w_mant_deta_condicioncalidad_obs
end type

type pb_siguiente from w_mant_detalle_csd`pb_siguiente within w_mant_deta_condicioncalidad_obs
end type

type pb_anterior from w_mant_detalle_csd`pb_anterior within w_mant_deta_condicioncalidad_obs
end type

type pb_primero from w_mant_detalle_csd`pb_primero within w_mant_deta_condicioncalidad_obs
end type

type pb_cancela from w_mant_detalle_csd`pb_cancela within w_mant_deta_condicioncalidad_obs
integer x = 2098
integer y = 360
end type

type pb_acepta from w_mant_detalle_csd`pb_acepta within w_mant_deta_condicioncalidad_obs
integer x = 2098
integer y = 144
end type

type pb_salir from w_mant_detalle_csd`pb_salir within w_mant_deta_condicioncalidad_obs
integer x = 2098
integer y = 576
end type

type dw_1 from w_mant_detalle_csd`dw_1 within w_mant_deta_condicioncalidad_obs
integer x = 82
integer width = 1920
integer height = 1028
string dataobject = "dw_mant_mues_condicioncalidad_obs"
end type

