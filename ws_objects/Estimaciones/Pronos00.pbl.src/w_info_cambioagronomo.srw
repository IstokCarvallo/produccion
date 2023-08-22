$PBExportHeader$w_info_cambioagronomo.srw
$PBExportComments$Informe con estimcion semanal
forward
global type w_info_cambioagronomo from w_para_informes
end type
type st_2 from statictext within w_info_cambioagronomo
end type
type st_3 from statictext within w_info_cambioagronomo
end type
type st_5 from statictext within w_info_cambioagronomo
end type
type st_7 from statictext within w_info_cambioagronomo
end type
type uo_selespecie from uo_seleccion_especie within w_info_cambioagronomo
end type
type st_1 from statictext within w_info_cambioagronomo
end type
type st_10 from statictext within w_info_cambioagronomo
end type
type uo_selpredios from uo_seleccion_predios within w_info_cambioagronomo
end type
type uo_selagronomo from uo_seleccion_agronomo within w_info_cambioagronomo
end type
type uo_selnuevo from uo_seleccion_agronomo within w_info_cambioagronomo
end type
type uo_selproductor from uo_seleccion_productor_agronomo within w_info_cambioagronomo
end type
end forward

global type w_info_cambioagronomo from w_para_informes
integer width = 2555
integer height = 1672
boolean minbox = false
boolean maxbox = false
st_2 st_2
st_3 st_3
st_5 st_5
st_7 st_7
uo_selespecie uo_selespecie
st_1 st_1
st_10 st_10
uo_selpredios uo_selpredios
uo_selagronomo uo_selagronomo
uo_selnuevo uo_selnuevo
uo_selproductor uo_selproductor
end type
global w_info_cambioagronomo w_info_cambioagronomo

type variables
DataWindowChild idwc_tempo
end variables

on w_info_cambioagronomo.create
int iCurrent
call super::create
this.st_2=create st_2
this.st_3=create st_3
this.st_5=create st_5
this.st_7=create st_7
this.uo_selespecie=create uo_selespecie
this.st_1=create st_1
this.st_10=create st_10
this.uo_selpredios=create uo_selpredios
this.uo_selagronomo=create uo_selagronomo
this.uo_selnuevo=create uo_selnuevo
this.uo_selproductor=create uo_selproductor
iCurrent=UpperBound(this.Control)
this.Control[iCurrent+1]=this.st_2
this.Control[iCurrent+2]=this.st_3
this.Control[iCurrent+3]=this.st_5
this.Control[iCurrent+4]=this.st_7
this.Control[iCurrent+5]=this.uo_selespecie
this.Control[iCurrent+6]=this.st_1
this.Control[iCurrent+7]=this.st_10
this.Control[iCurrent+8]=this.uo_selpredios
this.Control[iCurrent+9]=this.uo_selagronomo
this.Control[iCurrent+10]=this.uo_selnuevo
this.Control[iCurrent+11]=this.uo_selproductor
end on

on w_info_cambioagronomo.destroy
call super::destroy
destroy(this.st_2)
destroy(this.st_3)
destroy(this.st_5)
destroy(this.st_7)
destroy(this.uo_selespecie)
destroy(this.st_1)
destroy(this.st_10)
destroy(this.uo_selpredios)
destroy(this.uo_selagronomo)
destroy(this.uo_selnuevo)
destroy(this.uo_selproductor)
end on

event open;call super::open;Boolean	lb_Cerrar

IF IsNull(uo_SelAgronomo.codigo)	THEN lb_Cerrar	=	True
IF IsNull(uo_SelProductor.codigo)	THEN lb_Cerrar	=	True
IF IsNull(uo_SelPredios.codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelEspecie.codigo)		THEN lb_Cerrar	=	True
IF IsNull(uo_SelNuevo.codigo)		THEN lb_Cerrar	=	True

IF lb_Cerrar THEN
	Close(This)
ELSE
	uo_SelAgronomo.Seleccion(False, False)
	uo_SelProductor.Seleccion(True, False)
	uo_SelPredios.Seleccion(True, False)
	uo_SelEspecie.Seleccion(True, False)
	uo_SelNuevo.Seleccion(False, False)
	
	uo_SelProductor.Filtra(-1)
	
End If
end event

type pb_excel from w_para_informes`pb_excel within w_info_cambioagronomo
end type

type st_computador from w_para_informes`st_computador within w_info_cambioagronomo
end type

type st_usuario from w_para_informes`st_usuario within w_info_cambioagronomo
end type

type st_temporada from w_para_informes`st_temporada within w_info_cambioagronomo
end type

type p_logo from w_para_informes`p_logo within w_info_cambioagronomo
end type

type st_titulo from w_para_informes`st_titulo within w_info_cambioagronomo
integer x = 219
integer width = 1842
string text = "Cambio Asociación de Agronomo"
end type

type pb_acepta from w_para_informes`pb_acepta within w_info_cambioagronomo
integer x = 2153
integer y = 496
integer taborder = 100
string picturename = "\Desarrollo 17\Imagenes\Botones\Guardar Como.png"
string disabledname = "\Desarrollo 17\Imagenes\Botones\Guardar Como-bn.png"
end type

event pb_acepta::clicked;Long		ll_Productor, ll_Predio, ll_Agronomo
Integer	li_Especie, li_Nuevo

If IsNull(uo_SelAgronomo.Codigo) Or uo_SelAgronomo.Codigo = -1 Then
	MessageBox('Atencion', 'Debe seleccionar un agronomo para procesar.')
	Return
End If

If IsNull(uo_SelNuevo.Codigo) Or uo_SelNuevo.Codigo = -1 Then
	MessageBox('Atencion', 'Debe seleccionar un agronomo destino para procesar.')
	Return
End If

ll_Agronomo	= uo_SelAgronomo.Codigo
ll_Productor		= uo_SelProductor.Codigo
ll_Predio			= uo_SelPredios.Codigo
li_Especie		= uo_SelEspecie.Codigo
li_Nuevo			= uo_SelNuevo.Codigo

Declare Cambio Procedure For dbo.Pron_CambioAgronomo
	@Agronomo		=	:ll_Agronomo,
	@Productor		=	:ll_Productor,
	@Predio			=	:ll_Predio,
	@Especie		=	:li_Especie,
	@Agro 			=	:li_Nuevo
	Using SQLCA ;

Execute Cambio;	

If sqlca.SQLCode = -1 Then
	F_ErrorBaseDatos(sqlca,"Lectura del Procedimiento Almacenado Traspaso de asociacion de agronomos." )
Else
	MessageBox('Atención', 'Proceso finaliazado Exitosamente.', Exclamation!, Ok!)
End If

Close   Cambio;
end event

type pb_salir from w_para_informes`pb_salir within w_info_cambioagronomo
integer x = 2162
integer y = 940
integer taborder = 120
end type

type st_2 from statictext within w_info_cambioagronomo
integer x = 402
integer y = 1072
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Especie"
boolean focusrectangle = false
end type

type st_3 from statictext within w_info_cambioagronomo
integer x = 402
integer y = 524
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Agronomo"
boolean focusrectangle = false
end type

type st_5 from statictext within w_info_cambioagronomo
integer x = 402
integer y = 680
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Productor"
boolean focusrectangle = false
end type

type st_7 from statictext within w_info_cambioagronomo
integer x = 402
integer y = 1220
integer width = 571
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Nuevo Agronomo"
boolean focusrectangle = false
end type

type uo_selespecie from uo_seleccion_especie within w_info_cambioagronomo
event destroy ( )
string tag = "Seleccion de Especie"
integer x = 974
integer y = 1016
integer height = 180
integer taborder = 20
boolean bringtotop = true
end type

on uo_selespecie.destroy
call uo_seleccion_especie::destroy
end on

type st_1 from statictext within w_info_cambioagronomo
integer x = 219
integer y = 440
integer width = 1842
integer height = 952
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long backcolor = 16711680
alignment alignment = center!
boolean border = true
borderstyle borderstyle = styleraised!
boolean focusrectangle = false
end type

type st_10 from statictext within w_info_cambioagronomo
integer x = 402
integer y = 876
integer width = 347
integer height = 64
boolean bringtotop = true
integer textsize = -10
integer weight = 700
fontcharset fontcharset = ansi!
fontpitch fontpitch = variable!
fontfamily fontfamily = swiss!
string facename = "Arial"
long textcolor = 16777215
long backcolor = 553648127
string text = "Predio"
boolean focusrectangle = false
end type

type uo_selpredios from uo_seleccion_predios within w_info_cambioagronomo
integer x = 974
integer y = 820
integer height = 180
integer taborder = 70
boolean bringtotop = true
string text = ""
long tabtextcolor = 0
long picturemaskcolor = 0
end type

on uo_selpredios.destroy
call uo_seleccion_predios::destroy
end on

type uo_selagronomo from uo_seleccion_agronomo within w_info_cambioagronomo
integer x = 974
integer y = 516
integer height = 80
integer taborder = 110
boolean bringtotop = true
end type

on uo_selagronomo.destroy
call uo_seleccion_agronomo::destroy
end on

event ue_cambio;call super::ue_cambio;If IsNull(This.codigo) Then Return

Choose Case This.codigo
	Case -1, -9
		
	Case Else
		uo_SelProductor.Filtra(This.codigo)
		
End Choose
end event

type uo_selnuevo from uo_seleccion_agronomo within w_info_cambioagronomo
integer x = 974
integer y = 1212
integer height = 80
integer taborder = 120
boolean bringtotop = true
end type

on uo_selnuevo.destroy
call uo_seleccion_agronomo::destroy
end on

type uo_selproductor from uo_seleccion_productor_agronomo within w_info_cambioagronomo
integer x = 974
integer y = 624
integer height = 180
integer taborder = 130
boolean bringtotop = true
end type

event ue_cambio;call super::ue_cambio;If IsNull(This.codigo) Then Return

Choose Case This.codigo
	Case -1, -9
		
	Case Else
		uo_SelPredios.Filtra(This.codigo)
		
End Choose
end event

on uo_selproductor.destroy
call uo_seleccion_productor_agronomo::destroy
end on

