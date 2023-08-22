$PBExportHeader$w_info_despacho_nave_por_productor.srw
forward
global type w_info_despacho_nave_por_productor from w_info_saldos_globales
end type
type rb_1 from radiobutton within w_info_despacho_nave_por_productor
end type
type rb_2 from radiobutton within w_info_despacho_nave_por_productor
end type
type dw_embarques from datawindow within w_info_despacho_nave_por_productor
end type
type st_6 from statictext within w_info_despacho_nave_por_productor
end type
type st_7 from statictext within w_info_despacho_nave_por_productor
end type
type em_nro_embar from editmask within w_info_despacho_nave_por_productor
end type
type em_fech_embar from editmask within w_info_despacho_nave_por_productor
end type
type gb_emb from groupbox within w_info_despacho_nave_por_productor
end type
end forward

global type w_info_despacho_nave_por_productor from w_info_saldos_globales
integer width = 3438
integer height = 1356
string title = "DESPACHO DE NAVES POR PRODUCTOR"
rb_1 rb_1
rb_2 rb_2
dw_embarques dw_embarques
st_6 st_6
st_7 st_7
em_nro_embar em_nro_embar
em_fech_embar em_fech_embar
gb_emb gb_emb
end type
global w_info_despacho_nave_por_productor w_info_despacho_nave_por_productor

on w_info_despacho_nave_por_productor.create
int iCurrent
call super::create
this.rb_1=create rb_1
this.rb_2=create rb_2
this.dw_embarques=create dw_embarques
this.st_6=create st_6
this.st_7=create st_7
this.em_nro_embar=create em_nro_embar
this.em_fech_embar=create em_fech_embar
this.gb_emb=create gb_emb
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.rb_1
this.Control[iCurrent+2]=this.rb_2
this.Control[iCurrent+3]=this.dw_embarques
this.Control[iCurrent+4]=this.st_6
this.Control[iCurrent+5]=this.st_7
this.Control[iCurrent+6]=this.em_nro_embar
this.Control[iCurrent+7]=this.em_fech_embar
this.Control[iCurrent+8]=this.gb_emb
end on

on w_info_despacho_nave_por_productor.destroy
call super::destroy
destroy(this.rb_1)
destroy(this.rb_2)
destroy(this.dw_embarques)
destroy(this.st_6)
destroy(this.st_7)
destroy(this.em_nro_embar)
destroy(this.em_fech_embar)
destroy(this.gb_emb)
end on

type st_computador from w_info_saldos_globales`st_computador within w_info_despacho_nave_por_productor
end type

type st_usuario from w_info_saldos_globales`st_usuario within w_info_despacho_nave_por_productor
end type

type st_temporada from w_info_saldos_globales`st_temporada within w_info_despacho_nave_por_productor
end type

type p_logo from w_info_saldos_globales`p_logo within w_info_despacho_nave_por_productor
end type

type st_titulo from w_info_saldos_globales`st_titulo within w_info_despacho_nave_por_productor
string text = "Despacho Nave por Productor"
end type

type pb_acepta from w_info_saldos_globales`pb_acepta within w_info_despacho_nave_por_productor
integer x = 3086
integer y = 248
integer taborder = 90
boolean enabled = false
end type

event pb_acepta::clicked;Integer	fila
String	l_s_titulo

l_s_titulo	= 'Saldos por Partida'
istr_info.titulo	= 'Movimiento de Fruta Procesada '+l_s_titulo 

OpenWithParm(vinf, istr_info)
vinf.dw_1.DataObject = "dw_beta"
vinf.dw_1.SetTransObject(sqlca)
fila = vinf.dw_1.Retrieve()


IF fila = -1 THEN
	MessageBox( "Error en Base de Datos", "Se ha producido un error en Base " + &
				"de datos : ~n" + sqlca.SQLErrText, StopSign!, Ok!)
ELSEIF fila = 0 THEN
	MessageBox( "No Existe información", "No existe información para este informe.", &
	StopSign!, Ok!)
	ELSE
		F_Membrete(vinf.dw_1)
		IF gs_Ambiente <> 'Windows' THEN
			F_ImprimeInformePdf(vinf.dw_1, istr_info.titulo)
		END IF
END IF
SetPointer(Arrow!)				
end event

type pb_salir from w_info_saldos_globales`pb_salir within w_info_despacho_nave_por_productor
integer x = 3086
integer y = 580
integer taborder = 210
end type

type dw_etiqueta from w_info_saldos_globales`dw_etiqueta within w_info_despacho_nave_por_productor
end type

type dw_status from w_info_saldos_globales`dw_status within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 0
end type

type sle_vari_hasta from w_info_saldos_globales`sle_vari_hasta within w_info_despacho_nave_por_productor
end type

type sle_vari_desde from w_info_saldos_globales`sle_vari_desde within w_info_despacho_nave_por_productor
end type

type cb_1 from w_info_saldos_globales`cb_1 within w_info_despacho_nave_por_productor
end type

type cb_busc_var_d from w_info_saldos_globales`cb_busc_var_d within w_info_despacho_nave_por_productor
end type

type em_vari_hasta from w_info_saldos_globales`em_vari_hasta within w_info_despacho_nave_por_productor
integer taborder = 50
end type

type em_vari_desde from w_info_saldos_globales`em_vari_desde within w_info_despacho_nave_por_productor
end type

type st_5 from w_info_saldos_globales`st_5 within w_info_despacho_nave_por_productor
end type

type st_4 from w_info_saldos_globales`st_4 within w_info_despacho_nave_por_productor
integer x = 142
integer y = 720
end type

type st_3 from w_info_saldos_globales`st_3 within w_info_despacho_nave_por_productor
end type

type dw_especie from w_info_saldos_globales`dw_especie within w_info_despacho_nave_por_productor
end type

type st_2 from w_info_saldos_globales`st_2 within w_info_despacho_nave_por_productor
end type

type st_1 from w_info_saldos_globales`st_1 within w_info_despacho_nave_por_productor
end type

type dw_prod_hasta from w_info_saldos_globales`dw_prod_hasta within w_info_despacho_nave_por_productor
end type

type dw_prod_desde from w_info_saldos_globales`dw_prod_desde within w_info_despacho_nave_por_productor
end type

type gb_5 from w_info_saldos_globales`gb_5 within w_info_despacho_nave_por_productor
end type

type gb_4 from w_info_saldos_globales`gb_4 within w_info_despacho_nave_por_productor
end type

type gb_7 from w_info_saldos_globales`gb_7 within w_info_despacho_nave_por_productor
boolean visible = false
end type

type gb_8 from w_info_saldos_globales`gb_8 within w_info_despacho_nave_por_productor
integer x = 64
end type

type gb_6 from w_info_saldos_globales`gb_6 within w_info_despacho_nave_por_productor
boolean visible = false
end type

type cbx_todas from w_info_saldos_globales`cbx_todas within w_info_despacho_nave_por_productor
boolean visible = false
end type

type dw_planta from w_info_saldos_globales`dw_planta within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 120
end type

type gb_3 from w_info_saldos_globales`gb_3 within w_info_despacho_nave_por_productor
integer x = 1701
integer y = 160
integer height = 276
string text = "Pagina Productor"
end type

type cbx_calibre from w_info_saldos_globales`cbx_calibre within w_info_despacho_nave_por_productor
integer taborder = 200
end type

type cbx_serie from w_info_saldos_globales`cbx_serie within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 190
end type

type cbx_etiqueta from w_info_saldos_globales`cbx_etiqueta within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 180
end type

type cbx_status from w_info_saldos_globales`cbx_status within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 170
end type

type cbx_categoria from w_info_saldos_globales`cbx_categoria within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 160
end type

type cbx_envase from w_info_saldos_globales`cbx_envase within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 150
end type

type cbx_variedad from w_info_saldos_globales`cbx_variedad within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 140
end type

type cbx_productor from w_info_saldos_globales`cbx_productor within w_info_despacho_nave_por_productor
boolean visible = false
integer taborder = 130
end type

type cbx_prod from w_info_saldos_globales`cbx_prod within w_info_despacho_nave_por_productor
end type

type dw_vari from w_info_saldos_globales`dw_vari within w_info_despacho_nave_por_productor
end type

type cbx_vari from w_info_saldos_globales`cbx_vari within w_info_despacho_nave_por_productor
end type

type dw_1 from w_info_saldos_globales`dw_1 within w_info_despacho_nave_por_productor
integer x = 261
integer y = 244
end type

type gb_9 from w_info_saldos_globales`gb_9 within w_info_despacho_nave_por_productor
end type

type rb_1 from radiobutton within w_info_despacho_nave_por_productor
integer x = 1769
integer y = 260
integer width = 453
integer height = 76
integer taborder = 60
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "SI"
boolean lefttext = true
end type

type rb_2 from radiobutton within w_info_despacho_nave_por_productor
integer x = 2350
integer y = 260
integer width = 453
integer height = 76
integer taborder = 70
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "NO"
boolean lefttext = true
end type

type dw_embarques from datawindow within w_info_despacho_nave_por_productor
integer x = 1728
integer y = 568
integer width = 1051
integer height = 92
integer taborder = 80
boolean bringtotop = true
string dataobject = "dddw_embarques"
boolean border = false
boolean livescroll = true
end type

event constructor;Integer		li_fila
DataWindowChild dwchildembarque
InsertRow(0)

li_fila  = GetChild ('embq_codigo',dwchildembarque)

dwchildembarque.SetTransObject(sqlca)
dwchildembarque.Retrieve()
end event

event itemchanged;Date		ld_embq_fzarpe
Integer	li_oper_codigo

  CHOOSE CASE  dwo.name
			CASE 'embq_codigo'
					  SELECT "dba"."embarqueprod"."embq_fzarpe",   
					  		       "dba"."embarqueprod"."oper_codigo"  
									INTO :ld_embq_fzarpe,
										  :li_oper_codigo
								    FROM "dba"."embarqueprod"  
								   WHERE "dba"."embarqueprod"."embq_codigo" =:Data;
							em_nro_embar.Text  		=  String(li_oper_codigo)
							em_fech_embar.text		=  String(ld_embq_fzarpe)
COMMIT ;
END CHOOSE 

end event

type st_6 from statictext within w_info_despacho_nave_por_productor
integer x = 1728
integer y = 696
integer width = 398
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "N° Embarque"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_despacho_nave_por_productor
integer x = 2336
integer y = 696
integer width = 507
integer height = 76
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
boolean enabled = false
string text = "Fecha Embarque"
boolean focusrectangle = false
end type

type em_nro_embar from editmask within w_info_despacho_nave_por_productor
integer x = 1728
integer y = 780
integer width = 398
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
string mask = "#####"
end type

type em_fech_embar from editmask within w_info_despacho_nave_por_productor
integer x = 2400
integer y = 780
integer width = 375
integer height = 100
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
alignment alignment = center!
borderstyle borderstyle = stylelowered!
maskdatatype maskdatatype = datemask!
string mask = "dd/mm/yyyy"
end type

type gb_emb from groupbox within w_info_despacho_nave_por_productor
integer x = 1696
integer y = 492
integer width = 1243
integer height = 444
integer taborder = 90
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 33554432
long backcolor = 67108864
string text = "Embarque"
end type

